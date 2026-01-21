package com.card.management.batch.steps;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import com.card.management.DTOs.TransactionReportDetailDto;
import com.card.management.DTOs.TransactionReportResultDto;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Writer para el reporte de transacciones
 * Acumula datos en memoria para devolverlos como JSON
 * Equivalente a las rutinas de escritura de reporte en CBTRN03C
 * 
 * @StepScope garantiza que se cree una nueva instancia para cada ejecución del step
 */
@Component
@StepScope
@Slf4j
public class TransactionReportWriter implements ItemWriter<TransactionReportDetailDto> {
  
  // Mapa para agrupar transacciones por tarjeta
  private final Map<String, List<TransactionReportDetailDto>> transactionsByCard = new HashMap<>();
  private final Map<String, Long> accountIdByCard = new HashMap<>();
  private BigDecimal grandTotal = BigDecimal.ZERO;
  private long totalTransactionCount = 0;
  
  @Getter
  private TransactionReportResultDto reportResult;

  @Override
  public void write(@NonNull Chunk<? extends TransactionReportDetailDto> chunk) throws Exception {
    for (TransactionReportDetailDto detail : chunk) {
      processTransactionDetail(detail);
    }
  }

  /**
   * Procesa y acumula cada detalle de transacción en memoria
   */
  private void processTransactionDetail(TransactionReportDetailDto detail) {
    String cardNumber = detail.getCardNumber();
    
    // Agregar transacción al grupo de la tarjeta
    transactionsByCard.computeIfAbsent(cardNumber, k -> new ArrayList<>()).add(detail);
    
    // Guardar accountId por tarjeta
    accountIdByCard.putIfAbsent(cardNumber, detail.getAccountId());
    
    // Acumular totales
    grandTotal = grandTotal.add(detail.getAmount());
    totalTransactionCount++;
    
    log.debug("Processed transaction {} for card {}", detail.getTransactionId(), cardNumber);
  }

  /**
   * Construye el resultado final del reporte
   * Debe llamarse después de procesar todas las transacciones
   */
  public void buildReportResult() {
    List<TransactionReportResultDto.AccountTransactionGroup> accountGroups = new ArrayList<>();
    
    // Crear grupos por cuenta/tarjeta
    for (Map.Entry<String, List<TransactionReportDetailDto>> entry : transactionsByCard.entrySet()) {
      String cardNumber = entry.getKey();
      List<TransactionReportDetailDto> transactions = entry.getValue();
      
      // Calcular total de la cuenta
      BigDecimal accountTotal = transactions.stream()
          .map(TransactionReportDetailDto::getAmount)
          .reduce(BigDecimal.ZERO, BigDecimal::add);
      
      // Crear grupo de cuenta
      TransactionReportResultDto.AccountTransactionGroup group = 
          TransactionReportResultDto.AccountTransactionGroup.builder()
              .cardNumber(cardNumber)
              .accountId(accountIdByCard.get(cardNumber))
              .transactions(transactions)
              .accountTotal(accountTotal)
              .transactionCount(transactions.size())
              .build();
      
      accountGroups.add(group);
    }
    
    // Ordenar grupos por número de tarjeta
    accountGroups.sort((g1, g2) -> g1.getCardNumber().compareTo(g2.getCardNumber()));
    
    // Construir resultado final
    reportResult = TransactionReportResultDto.builder()
        .accountGroups(accountGroups)
        .grandTotal(grandTotal)
        .totalTransactionCount(totalTransactionCount)
        .accountCount(accountGroups.size())
        .build();
    
    log.info("Transaction report built. Accounts: {}, Total transactions: {}, Grand total: {}", 
        accountGroups.size(), totalTransactionCount, grandTotal);
  }
  
  /**
   * Reinicia el writer para una nueva ejecución
   */
  public void reset() {
    transactionsByCard.clear();
    accountIdByCard.clear();
    grandTotal = BigDecimal.ZERO;
    totalTransactionCount = 0;
    reportResult = null;
    log.debug("TransactionReportWriter reset completed");
  }
}
