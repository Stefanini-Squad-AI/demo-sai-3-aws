package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

/**
 * Entidad JPA que representa una transacción rechazada
 * Migrada desde estructura COBOL REJECT-RECORD (DALYREJS-FILE)
 * Equivalente a FD-REJS-RECORD en COBOL
 */
@Entity
@Table(name = "REJECTED_TRANSACTIONS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RejectedTransaction {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "TRANSACTION_DATA", length = 350, nullable = false)
  private String transactionData; // REJECT-TRAN-DATA PIC X(350) - datos completos de la transacción original

  @Column(name = "VALIDATION_FAILURE_REASON", nullable = false)
  private Integer validationFailureReason; // WS-VALIDATION-FAIL-REASON PIC 9(04)

  @Column(name = "VALIDATION_FAILURE_DESCRIPTION", length = 76)
  private String validationFailureDescription; // WS-VALIDATION-FAIL-REASON-DESC PIC X(76)

  @Column(name = "REJECTED_TIMESTAMP")
  private LocalDateTime rejectedTimestamp; // Timestamp cuando se rechazó

  @Column(name = "ORIGINAL_TRANSACTION_ID", length = 16)
  private String originalTransactionId; // Para referencia
}
