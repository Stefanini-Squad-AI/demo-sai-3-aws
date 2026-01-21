package com.card.management.batch.steps;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.card.management.Models.Account;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.DisclosureGroup;
import com.card.management.Models.InterestCalculationResult;
import com.card.management.Models.TransactionCategoryBalance;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.CardXrefRecordRepository;
import com.card.management.Repositories.DisclosureGroupRepository;

import lombok.extern.slf4j.Slf4j;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Optional;

/**
 * Processor que calcula intereses para cada balance de categoría de transacción
 * Equivalente a la lógica principal del loop en CBACT04C
 */
@Component
@Slf4j
public class InterestCalculationProcessor
    implements ItemProcessor<TransactionCategoryBalance, InterestCalculationResult> {
  @Autowired
  private AccountRepository accountRepository;

  @Autowired
  private CardXrefRecordRepository cardXrefRepository;

  @Autowired
  private DisclosureGroupRepository disclosureGroupRepository;

  private Long lastAccountNumber = 0L;
  private BigDecimal totalInterest = BigDecimal.ZERO;
  private Account currentAccount;
  private CardXrefRecord currentXref;
  private int transactionIdSuffix = 0;

  @Override
  public InterestCalculationResult process(TransactionCategoryBalance tcatBalance) throws Exception {

    log.debug("Processing transaction category balance: {}", tcatBalance);

    // Equivalente a: IF TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM
    if (!tcatBalance.getAccountId().equals(lastAccountNumber)) {

      // Si no es la primera vez, actualizar la cuenta anterior
      if (!lastAccountNumber.equals(0L)) {
        updatePreviousAccount();
      }

      // Resetear totales para nueva cuenta
      totalInterest = BigDecimal.ZERO;
      lastAccountNumber = tcatBalance.getAccountId();

      // Obtener datos de la cuenta - Equivalente a: PERFORM 1100-GET-ACCT-DATA
      currentAccount = getAccountData(tcatBalance.getAccountId());

      // Obtener datos de referencia cruzada - Equivalente a: PERFORM
      // 1110-GET-XREF-DATA
      currentXref = getXrefData(tcatBalance.getAccountId());
    }

    // Obtener tasa de interés - Equivalente a: PERFORM 1200-GET-INTEREST-RATE
    BigDecimal interestRate = getInterestRate(currentAccount.getGroupId(),
        tcatBalance.getTypeCode(),
        tcatBalance.getCategoryCode());

    InterestCalculationResult result = null;

    // Si hay tasa de interés, calcular interés - Equivalente a: IF DIS-INT-RATE NOT
    // = 0
    if (interestRate.compareTo(BigDecimal.ZERO) != 0) {
      result = computeInterest(tcatBalance, interestRate);
      // PERFORM 1400-COMPUTE-FEES sería implementado aquí si fuera necesario
    }

    return result;
  }

  /**
   * Obtiene datos de la cuenta
   * Equivalente a: 1100-GET-ACCT-DATA
   */
  private Account getAccountData(Long accountId) {
    Optional<Account> account = accountRepository.findById(accountId);
    if (account.isEmpty()) {
      log.error("ACCOUNT NOT FOUND: {}", accountId);
      throw new RuntimeException("Account not found: " + accountId);
    }
    return account.get();
  }

  /**
   * Obtiene datos de referencia cruzada
   * Equivalente a: 1110-GET-XREF-DATA
   */
  private CardXrefRecord getXrefData(Long accountId) {
    Optional<CardXrefRecord> xref = cardXrefRepository.findByAccountId(accountId);
    if (xref.isEmpty()) {
      log.error("ACCOUNT NOT FOUND IN XREF: {}", accountId);
      throw new RuntimeException("Account not found in xref: " + accountId);
    }
    return xref.get();
  }

  /**
   * Obtiene la tasa de interés del grupo de divulgación
   * Equivalente a: 1200-GET-INTEREST-RATE y 1200-A-GET-DEFAULT-INT-RATE
   */
  private BigDecimal getInterestRate(String accountGroupId, String transactionTypeCode,
      Integer transactionCategoryCode) {

    // Buscar tasa específica del grupo
    Optional<DisclosureGroup> disclosureGroup = disclosureGroupRepository
        .findByAccountGroupIdAndTransactionTypeCodeAndTransactionCategoryCode(
            accountGroupId, transactionTypeCode, transactionCategoryCode);

    if (disclosureGroup.isPresent()) {
      return disclosureGroup.get().getInterestRate();
    }

    // Si no se encuentra, buscar con grupo DEFAULT
    log.debug("DISCLOSURE GROUP RECORD MISSING, TRY WITH DEFAULT GROUP CODE");
    Optional<DisclosureGroup> defaultGroup = disclosureGroupRepository
        .findByAccountGroupIdAndTransactionTypeCodeAndTransactionCategoryCode(
            "DEFAULT", transactionTypeCode, transactionCategoryCode);

    if (defaultGroup.isPresent()) {
      return defaultGroup.get().getInterestRate();
    }

    log.error("ERROR READING DEFAULT DISCLOSURE GROUP");
    throw new RuntimeException("Default disclosure group not found");
  }

  /**
   * Calcula el interés mensual
   * Equivalente a: 1300-COMPUTE-INTEREST
   */
  private InterestCalculationResult computeInterest(TransactionCategoryBalance tcatBalance, BigDecimal interestRate) {

    // COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
    BigDecimal monthlyInterest = tcatBalance.getCategoryBalance()
        .multiply(interestRate)
        .divide(new BigDecimal("1200"), 2, RoundingMode.HALF_UP);

    // ADD WS-MONTHLY-INT TO WS-TOTAL-INT
    totalInterest = totalInterest.add(monthlyInterest);

    // Incrementar sufijo de ID de transacción
    transactionIdSuffix++;

    // Crear resultado con datos para la transacción
    return InterestCalculationResult.builder()
        .accountId(tcatBalance.getAccountId())
        .transactionId(generateTransactionId())
        .interestAmount(monthlyInterest)
        .totalInterest(totalInterest)
        .cardNumber(currentXref.getCardNumber())
        .account(currentAccount)
        .build();
  }

  /**
   * Genera ID de transacción
   * Equivalente a la lógica STRING en 1300-B-WRITE-TX
   */
  private String generateTransactionId() {
    // Asumiendo que tenemos acceso a la fecha del parámetro del job
    String paramDate = "2022071800"; // Esto vendría del JobParameter
    return paramDate + String.format("%06d", transactionIdSuffix);
  }

  /**
   * Actualiza la cuenta anterior con el total de intereses
   * Equivalente a: 1050-UPDATE-ACCOUNT
   */
  private void updatePreviousAccount() {
    if (currentAccount != null && totalInterest.compareTo(BigDecimal.ZERO) > 0) {
      // ADD WS-TOTAL-INT TO ACCT-CURR-BAL
      currentAccount.setCurrentBalance(
          currentAccount.getCurrentBalance().add(totalInterest));

      // MOVE 0 TO ACCT-CURR-CYC-CREDIT y ACCT-CURR-CYC-DEBIT
      currentAccount.setCurrentCycleCredit(BigDecimal.ZERO);
      currentAccount.setCurrentCycleDebit(BigDecimal.ZERO);

      accountRepository.save(currentAccount);
    }
  }
}
