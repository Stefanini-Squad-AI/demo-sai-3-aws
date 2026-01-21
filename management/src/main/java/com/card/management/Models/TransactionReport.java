package com.card.management.Models;

import java.math.BigDecimal;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Data
@Table(name = "TRANSACTION_REPORT")
public class TransactionReport {
  @Id
  @Column(name = "COMPOSITE_KEY", length = 32)
  private String compositeKey; // CARD_NUM + TRAN_ID

  @Column(name = "CARD_NUM", length = 16)
  private String cardNumber;

  @Column(name = "TRAN_ID", length = 16)
  private String transactionId;

  @Column(name = "TRAN_DESC", length = 50)
  private String transactionDescription;

  @Column(name = "TRAN_AMT", precision = 11, scale = 2)
  private BigDecimal transactionAmount;

  @Column(name = "TRAN_REST", length = 318)
  private String transactionRest; // Resto de datos de la transacción
}
