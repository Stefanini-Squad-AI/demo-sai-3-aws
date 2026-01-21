package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Entidad JPA que representa un registro de cuenta
 * Migrada desde la estructura COBOL ACCOUNT-RECORD (RECLN 300)
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
 */
@Entity
@Table(name = "ACCOUNT")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Account {
  /**
   * ID de cuenta - COBOL: ACCT-ID PIC 9(11)
   * Campo numérico de 11 dígitos
   */
  @Id
  @Column(name = "ACCT_ID", precision = 11, scale = 0)
  private Long accountId;

  /**
   * Estado activo de la cuenta - COBOL: ACCT-ACTIVE-STATUS PIC X(01)
   * Campo alfanumérico de 1 carácter
   */
  @Column(name = "ACCT_ACTIVE_STATUS", length = 1)
  private String activeStatus;

  /**
   * Balance actual - COBOL: ACCT-CURR-BAL PIC S9(10)V99
   * Campo numérico con signo, 10 dígitos enteros y 2 decimales
   */
  @Column(name = "ACCT_CURR_BAL", precision = 12, scale = 2)
  private BigDecimal currentBalance;

  /**
   * Límite de crédito - COBOL: ACCT-CREDIT-LIMIT PIC S9(10)V99
   * Campo numérico con signo, 10 dígitos enteros y 2 decimales
   */
  @Column(name = "ACCT_CREDIT_LIMIT", precision = 12, scale = 2)
  private BigDecimal creditLimit;

  /**
   * Límite de crédito en efectivo - COBOL: ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
   * Campo numérico con signo, 10 dígitos enteros y 2 decimales
   */
  @Column(name = "ACCT_CASH_CREDIT_LIMIT", precision = 12, scale = 2)
  private BigDecimal cashCreditLimit;

  /**
   * Fecha de apertura - COBOL: ACCT-OPEN-DATE PIC X(10)
   * Campo alfanumérico de 10 caracteres convertido a LocalDate
   */
  @Column(name = "ACCT_OPEN_DATE")
  private LocalDate openDate;

  /**
   * Fecha de expiración - COBOL: ACCT-EXPIRAION-DATE PIC X(10)
   * Campo alfanumérico de 10 caracteres convertido a LocalDate
   * Nota: Se corrigió el typo "EXPIRAION" a "EXPIRATION"
   */
  @Column(name = "ACCT_EXPIRATION_DATE")
  private LocalDate expirationDate;

  /**
   * Fecha de reemisión - COBOL: ACCT-REISSUE-DATE PIC X(10)
   * Campo alfanumérico de 10 caracteres convertido a LocalDate
   */
  @Column(name = "ACCT_REISSUE_DATE")
  private LocalDate reissueDate;

  /**
   * Crédito del ciclo actual - COBOL: ACCT-CURR-CYC-CREDIT PIC S9(10)V99
   * Campo numérico con signo, 10 dígitos enteros y 2 decimales
   */
  @Column(name = "ACCT_CURR_CYC_CREDIT", precision = 12, scale = 2)
  private BigDecimal currentCycleCredit;

  /**
   * Débito del ciclo actual - COBOL: ACCT-CURR-CYC-DEBIT PIC S9(10)V99
   * Campo numérico con signo, 10 dígitos enteros y 2 decimales
   */
  @Column(name = "ACCT_CURR_CYC_DEBIT", precision = 12, scale = 2)
  private BigDecimal currentCycleDebit;

  /**
   * Código postal de la dirección - COBOL: ACCT-ADDR-ZIP PIC X(10)
   * Campo alfanumérico de 10 caracteres
   */
  @Column(name = "ACCT_ADDR_ZIP", length = 10)
  private String addressZipCode;

  /**
   * ID del grupo - COBOL: ACCT-GROUP-ID PIC X(10)
   * Campo alfanumérico de 10 caracteres
   */
  @Column(name = "ACCT_GROUP_ID", length = 10)
  private String groupId;

  // El FILLER de 178 caracteres no se migra ya que representa espacio no
  // utilizado
}
