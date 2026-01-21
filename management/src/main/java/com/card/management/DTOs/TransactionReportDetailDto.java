package com.card.management.DTOs;

import lombok.Builder;
import lombok.Data;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO para el detalle del reporte de transacciones
 * Equivalente a TRANSACTION-DETAIL-REPORT en COBOL
 */
@Data
@Builder
public class TransactionReportDetailDto {
  private String transactionId; // TRAN-REPORT-TRANS-ID
  private Long accountId; // TRAN-REPORT-ACCOUNT-ID
  private String cardNumber; // Para control break
  private String typeCode; // TRAN-REPORT-TYPE-CD
  private String typeDescription; // TRAN-REPORT-TYPE-DESC
  private Integer categoryCode; // TRAN-REPORT-CAT-CD
  private String categoryDescription; // TRAN-REPORT-CAT-DESC
  private String source; // TRAN-REPORT-SOURCE
  private BigDecimal amount; // TRAN-REPORT-AMT
  private LocalDateTime processedTimestamp;
}
