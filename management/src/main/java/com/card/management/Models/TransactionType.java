package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

/**
 * Entidad que representa los tipos de transacción del sistema CardDemo
 * Migrada desde estructura COBOL TRAN-TYPE-RECORD (RECLN = 60)
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
 */
@Entity
@Table(name = "TRANSACTION_TYPE")
@Data // Genera getters, setters, toString, equals y hashCode
@NoArgsConstructor // Constructor sin parámetros requerido por JPA
@AllArgsConstructor // Constructor con todos los parámetros
public class TransactionType {
  /**
   * Código del tipo de transacción (equivalente a TRAN-TYPE PIC X(02))
   * Clave primaria de 2 caracteres
   */
  @Id
  @Column(name = "TRAN_TYPE", length = 2, nullable = false)
  private String transactionTypeCode;

  /**
   * Descripción del tipo de transacción (equivalente a TRAN-TYPE-DESC PIC X(50))
   * Campo descriptivo de hasta 50 caracteres
   */
  @Column(name = "TRAN_TYPE_DESC", length = 50, nullable = false)
  private String transactionTypeDescription;

  // Relación 1:N con TransactionRecord - Un tipo puede tener múltiples
  // transacciones
  @OneToMany(mappedBy = "transactionType", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<TransactionRecord> transactions;

  // Nota: El FILLER PIC X(08) del COBOL no se migra ya que era espacio de relleno
  // sin valor funcional, usado para completar los 60 bytes del registro
  public TransactionType(String trType, String trDescription) {
    this.transactionTypeCode = trType;
    this.transactionTypeDescription = trDescription;
  }
}
