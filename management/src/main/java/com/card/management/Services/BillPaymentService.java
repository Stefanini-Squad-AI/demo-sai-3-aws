package com.card.management.Services;

import com.card.management.DTOs.BillPaymentRequestDto;
import com.card.management.DTOs.BillPaymentResponseDto;
import com.card.management.Models.Account;
import com.card.management.Models.TransactionRecord;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.TransactionRecordRepository;
import com.card.management.Repositories.CardXrefRecordRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class BillPaymentService {
  private final AccountRepository accountRepository;
  private final TransactionRecordRepository transactionRecordRepository;
  private final CardXrefRecordRepository cardXrefRecordRepository;

  /**
   * Procesa el pago de factura completo del saldo de la cuenta
   * Equivalente a PROCESS-ENTER-KEY en COBOL
   */
  @Transactional
  public BillPaymentResponseDto processBillPayment(BillPaymentRequestDto request) {

    BillPaymentResponseDto response = new BillPaymentResponseDto();

    try {
      // Validación de Account ID (equivalente a validación en COBOL)
      if (request.getAccountId() == null) {
        response.setSuccess(false);
        response.setErrorMessage("Acct ID can NOT be empty...");
        return response;
      }

        // Leer cuenta (equivalente a READ-ACCTDAT-FILE)
        Account account = accountRepository.findById(request.getAccountId())
            .orElseThrow(() -> new RuntimeException("Account ID NOT found..."));

        response.setAccountId(account.getAccountId());
        response.setCurrentBalance(account.getCurrentBalance());

      // Validar que hay saldo para pagar
      if (account.getCurrentBalance().compareTo(BigDecimal.ZERO) <= 0) {
        response.setSuccess(false);
        response.setErrorMessage("You have nothing to pay...");
        return response;
      }

      // Procesar confirmación
      if ("Y".equalsIgnoreCase(request.getConfirmation()) ||
          "y".equals(request.getConfirmation())) {

        // Obtener referencia cruzada de tarjeta (equivalente a READ-CXACAIX-FILE)
        CardXrefRecord cardXref = cardXrefRecordRepository
            .findByAccountId(request.getAccountId())
            .orElseThrow(() -> new RuntimeException("Account ID NOT found..."));

        // Generar nuevo ID de transacción (equivalente a lógica STARTBR/READPREV)
        Long nextTransactionId = transactionRecordRepository.findLastTransaction()
          .map(transaction -> Long.parseLong(transaction.getTransactionId()) + 1)
          .orElse(1L);

        // Crear transacción de pago (equivalente a WRITE-TRANSACT-FILE)
        TransactionRecord transaction = new TransactionRecord();
        transaction.setTransactionId(String.valueOf(nextTransactionId));
        transaction.setTransactionTypeCode("02");
        transaction.setTransactionCategoryCode(2); // Integer value
        transaction.setTransactionSource("POS TERM");
        transaction.setTransactionDescription("BILL PAYMENT - ONLINE");
        transaction.setTransactionAmount(account.getCurrentBalance());
        transaction.setCardNumber(cardXref.getCardNumber());
        transaction.setMerchantId(999999999L);
        transaction.setMerchantName("BILL PAYMENT");
        transaction.setMerchantCity("N/A");
        transaction.setMerchantZipCode("N/A");

        LocalDateTime currentTimestamp = LocalDateTime.now();
        transaction.setOriginalTimestamp(currentTimestamp);
        transaction.setProcessedTimestamp(currentTimestamp);

        transactionRecordRepository.save(transaction);

        // Actualizar saldo de cuenta (equivalente a UPDATE-ACCTDAT-FILE)
        BigDecimal paymentAmount = account.getCurrentBalance();
        account.setCurrentBalance(account.getCurrentBalance().subtract(paymentAmount));
        accountRepository.save(account);

        response.setPaymentAmount(paymentAmount);
        response.setTransactionId(nextTransactionId);
        response.setCurrentBalance(account.getCurrentBalance());
        response.setSuccess(true);
        response.setMessage("Payment successful. Your Transaction ID is " + nextTransactionId + ".");

      } else if ("N".equalsIgnoreCase(request.getConfirmation()) ||
          "n".equals(request.getConfirmation())) {
        response.setSuccess(false);
        response.setErrorMessage("Payment cancelled by user");

      } else if (request.getConfirmation() == null ||
          request.getConfirmation().trim().isEmpty()) {
        response.setSuccess(false);
        response.setMessage("Confirm to make a bill payment...");

      } else {
        response.setSuccess(false);
        response.setErrorMessage("Invalid value. Valid values are (Y/N)...");
      }

    } catch (Exception e) {
      response.setSuccess(false);
      response.setErrorMessage("Unable to process bill payment: " + e.getMessage());
    }

    return response;
  }

  public BillPaymentResponseDto consultBalance(Long accountId) {
    BillPaymentResponseDto response = new BillPaymentResponseDto();

    try {
      // Validación de Account ID (equivalente a validación en COBOL)
      if (accountId == null) {
        response.setSuccess(false);
        response.setErrorMessage("Acct ID can NOT be empty...");
        return response;
      }

      // Leer cuenta (equivalente a READ-ACCTDAT-FILE)
      Account account = accountRepository.findById(accountId)
        .orElseThrow(() -> new RuntimeException("Account ID NOT found..."));

      response.setAccountId(account.getAccountId());
      response.setCurrentBalance(account.getCurrentBalance());
      response.setSuccess(true);
      response.setMessage("Confirm to make a bill payment...");

    } catch (Exception e) {
      response.setSuccess(false);
      response.setErrorMessage("Unable to consult balance: " + e.getMessage());
    }

    return response;
  }
}