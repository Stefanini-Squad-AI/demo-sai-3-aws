package com.card.management.Services;

import com.card.management.DTOs.TransactionAddRequestDto;
import com.card.management.DTOs.TransactionAddResponseDto;
import com.card.management.Models.TransactionRecord;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Repositories.TransactionRecordRepository;
import com.card.management.Repositories.CardXrefRecordRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionAddService {
  private final TransactionRecordRepository transactionRecordRepository;
  private final CardXrefRecordRepository cardXrefRecordRepository;

  @Transactional
  public TransactionAddResponseDto addTransaction(TransactionAddRequestDto request) {
    try {
      // Validar confirmación
      if (!"Y".equalsIgnoreCase(request.getConfirmation())) {
        return TransactionAddResponseDto.error("Confirm to add this transaction...");
      }

      // Validar y obtener información de cuenta/tarjeta
      String cardNumber = validateAndGetCardNumber(request);
      if (cardNumber == null) {
        return TransactionAddResponseDto.error("Account or Card Number must be entered...");
      }

      // Generar nuevo ID de transacción
      String newTransactionId = generateNextTransactionId();

      // Crear nueva transacción
      TransactionRecord transaction = createTransaction(request, cardNumber, newTransactionId);

      // Guardar transacción
      transactionRecordRepository.save(transaction);

      log.info("Transaction added successfully with ID: {}", newTransactionId);
      return TransactionAddResponseDto.success(newTransactionId);

    } catch (Exception e) {
      log.error("Error adding transaction", e);
      return TransactionAddResponseDto.error("Unable to Add Transaction...");
    }
  }

  private String validateAndGetCardNumber(TransactionAddRequestDto request) {
    // Si se proporciona Account ID
    if (StringUtils.hasText(request.getAccountId())) {
      if (!request.getAccountId().matches("\\d{1,11}")) {
        throw new IllegalArgumentException("Account ID must be Numeric...");
      }

      CardXrefRecord cardXref = cardXrefRecordRepository.findByAccountId(Long.valueOf(request.getAccountId()))
          .orElseThrow(() -> new IllegalArgumentException("Account ID NOT found..."));

      return cardXref.getCardNumber();
    }

    // Si se proporciona Card Number
    if (StringUtils.hasText(request.getCardNumber())) {
      if (!request.getCardNumber().matches("\\d{1,16}")) {
        throw new IllegalArgumentException("Card Number must be Numeric...");
      }

      CardXrefRecord cardXref = cardXrefRecordRepository.findByCardNumber(request.getCardNumber())
          .orElseThrow(() -> new IllegalArgumentException("Card Number NOT found..."));

      return cardXref.getCardNumber();
    }

    return null;
  }

  private String generateNextTransactionId() {
    // Equivalente a la lógica COBOL de obtener el último ID y sumar 1
    return transactionRecordRepository.findLastTransaction()
        .map(transaction -> String.valueOf(Long.parseLong(transaction.getTransactionId()) + 1))
        .orElse("1");
  }

  private TransactionRecord createTransaction(TransactionAddRequestDto request, String cardNumber, String transactionId) {
    TransactionRecord transaction = new TransactionRecord();
    transaction.setTransactionId(transactionId);
    transaction.setTransactionTypeCode(request.getTransactionTypeCode());
    transaction.setTransactionCategoryCode(request.getTransactionCategoryCode());
    transaction.setTransactionSource(request.getTransactionSource());
    transaction.setTransactionDescription(request.getTransactionDescription());
    transaction.setTransactionAmount(request.getTransactionAmount());
    transaction.setCardNumber(cardNumber);
    transaction.setMerchantId(Long.valueOf(request.getMerchantId()));
    transaction.setMerchantName(request.getMerchantName());
    transaction.setMerchantCity(request.getMerchantCity());
    transaction.setMerchantZipCode(request.getMerchantZip());
    transaction.setOriginalTimestamp(request.getOriginalDate());
    transaction.setProcessedTimestamp(request.getProcessDate());

    return transaction;
  }
}