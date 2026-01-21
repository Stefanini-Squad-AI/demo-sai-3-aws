package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

/**
 * Entidad JPA que representa la estructura de referencia cruzada de tarjetas
 * Migrada desde COBOL CARD-XREF-RECORD (RECLN 50)
 * 
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
 */
@Entity
@Table(name = "CARD_XREF")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CardXrefRecord {
  /**
   * Número de tarjeta - equivalente a XREF-CARD-NUM PIC X(16)
   * Campo clave primaria para identificar únicamente cada registro
   */
  @Id
  @Column(name = "XREF_CARD_NUM", length = 16, nullable = false)
  private String cardNumber;

  /**
   * ID del cliente - equivalente a XREF-CUST-ID PIC 9(09)
   * Referencia al cliente propietario de la tarjeta
   */
  @Column(name = "XREF_CUST_ID", precision = 9, nullable = false)
  private Long customerId;

  /**
   * ID de la cuenta - equivalente a XREF-ACCT-ID PIC 9(11)
   * Referencia a la cuenta asociada con la tarjeta
   */
  @Column(name = "XREF_ACCT_ID", precision = 11, nullable = false)
  private Long accountId;

  /**
   * Campo de relleno - equivalente a FILLER PIC X(14)
   * Mantenido para compatibilidad con la estructura original de 50 caracteres
   * En la implementación Java, este campo puede ser omitido o usado para futuros
   * campos
   */
  @Column(name = "FILLER", length = 14)
  private String filler;

  /**
   * Campos de auditoría adicionales recomendados para aplicaciones modernas
   * Estos no existían en la estructura COBOL original pero son buenas prácticas
   */
  @Column(name = "CREATED_DATE", updatable = false)
  @Temporal(TemporalType.TIMESTAMP)
  private java.util.Date createdDate;

  @Column(name = "UPDATED_DATE")
  @Temporal(TemporalType.TIMESTAMP)
  private java.util.Date updatedDate;

  /**
   * Método de callback para establecer fecha de creación automáticamente
   */
  @PrePersist
  protected void onCreate() {
    createdDate = new java.util.Date();
    updatedDate = new java.util.Date();
  }

  /**
   * Método de callback para actualizar fecha de modificación automáticamente
   */
  @PreUpdate
  protected void onUpdate() {
    updatedDate = new java.util.Date();
  }

  // Relación N:1 con Customer - Múltiples xref pueden referenciar al mismo customer
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "XREF_CUST_ID", insertable = false, updatable = false)
  private Customer customer;

  // Relación N:1 con Card - Múltiples xref pueden referenciar a la misma card
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "XREF_CARD_NUM", insertable = false, updatable = false)
  private Card card;

  // Relación N:1 con Account - Múltiples xref pueden referenciar a la misma account
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "XREF_ACCT_ID", insertable = false, updatable = false)
  private Account account;
}
