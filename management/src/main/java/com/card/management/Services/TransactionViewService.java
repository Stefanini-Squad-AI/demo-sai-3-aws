package com.card.management.Services;

import com.card.management.DTOs.TransactionViewResponseDto;
import com.card.management.Models.TransactionRecord;
import com.card.management.Repositories.TransactionRecordRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

/**
 * Servicio que maneja la lógica de negocio para visualización de transacciones
 * Migrado desde las funciones PROCESS-ENTER-KEY y READ-TRANSACT-FILE del COBOL
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionViewService {
  private final TransactionRecordRepository transactionRecordRepository;

  private static final String PROGRAM_NAME = "COTRN01C";
  private static final String TRANSACTION_NAME = "CT01";

  /**
   * Busca y retorna los detalles de una transacción
   * Equivalente a PROCESS-ENTER-KEY y READ-TRANSACT-FILE
   */
  public TransactionViewResponseDto getTransactionDetails(String transactionId) {
    TransactionViewResponseDto dto = new TransactionViewResponseDto();

    // Validación equivalente a la validación de TRNIDINI en COBOL
    if (transactionId == null || transactionId.trim().isEmpty()) {
      dto.setErrorMessage("Transaction ID cannot be empty...");
      return dto;
    }

    try {
      // Equivalente a READ-TRANSACT-FILE
      Optional<TransactionRecord> transactionOpt = transactionRecordRepository.findById(transactionId.trim());

      if (transactionOpt.isPresent()) {
        TransactionRecord transaction = transactionOpt.get();

        // Mapeo de datos equivalente a los MOVE statements del COBOL
        dto.setTransactionId(transaction.getTransactionId());
        dto.setCardNumber(transaction.getCardNumber());
        dto.setTransactionTypeCode(transaction.getTransactionTypeCode());
        dto.setTransactionCategoryCode(transaction.getTransactionCategoryCode());
        dto.setTransactionSource(transaction.getTransactionSource());
        dto.setTransactionAmount(transaction.getTransactionAmount());
        dto.setTransactionDescription(transaction.getTransactionDescription());
        dto.setOriginalTimestamp(transaction.getOriginalTimestamp());
        dto.setProcessedTimestamp(transaction.getProcessedTimestamp());
        dto.setMerchantId(String.valueOf(transaction.getMerchantId()));
        dto.setMerchantName(transaction.getMerchantName());
        dto.setMerchantCity(transaction.getMerchantCity());
        dto.setMerchantZip(transaction.getMerchantZipCode());

        // Equivalente a POPULATE-HEADER-INFO
        populateHeaderInfo(dto);

      } else {
        // Equivalente a DFHRESP(NOTFND)
        dto.setErrorMessage("Transaction ID NOT found...");
        log.warn("Transaction not found: {}", transactionId);
      }

    } catch (Exception e) {
      // Equivalente al manejo de errores en READ-TRANSACT-FILE
      dto.setErrorMessage("Unable to lookup Transaction...");
      log.error("Error retrieving transaction {}: {}", transactionId, e.getMessage(), e);
    }

    return dto;
  }

  /**
   * Inicializa un DTO vacío para nueva búsqueda
   * Equivalente a INITIALIZE-ALL-FIELDS y CLEAR-CURRENT-SCREEN
   */
  public TransactionViewResponseDto initializeEmptyView() {
    TransactionViewResponseDto dto = new TransactionViewResponseDto();
    populateHeaderInfo(dto);
    return dto;
  }

  /**
   * Puebla la información del header
   * Equivalente a POPULATE-HEADER-INFO del COBOL
   */
  private void populateHeaderInfo(TransactionViewResponseDto dto) {
    LocalDateTime now = LocalDateTime.now();

    dto.setCurrentDate(now.format(DateTimeFormatter.ofPattern("MM/dd/yy")));
    dto.setCurrentTime(now.format(DateTimeFormatter.ofPattern("HH:mm:ss")));
    dto.setProgramName(PROGRAM_NAME);
    dto.setTransactionName(TRANSACTION_NAME);
  }
}
