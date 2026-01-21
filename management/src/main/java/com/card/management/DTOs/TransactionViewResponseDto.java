package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO para transferir datos de transacción a la vista
 * Representa los campos de salida del mapa COTRN1AO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionViewResponseDto {
  private String transactionId;
  private String cardNumber;
  private String transactionTypeCode;
  private Integer transactionCategoryCode;
  private String transactionSource;
  private BigDecimal transactionAmount;
  private String transactionDescription;
  private LocalDateTime originalTimestamp;
  private LocalDateTime processedTimestamp;
  private String merchantId;
  private String merchantName;
  private String merchantCity;
  private String merchantZip;
  private String errorMessage;

  // Campos de header equivalentes a POPULATE-HEADER-INFO
  private String currentDate;
  private String currentTime;
  private String programName;
  private String transactionName;
}
