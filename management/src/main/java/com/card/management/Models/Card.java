package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import java.time.LocalDate;
import java.util.List;

/**
 * Entidad JPA que representa una tarjeta de crédito/débito
 * Migrada desde COBOL CARD-RECORD structure
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
 */
@Entity
@Table(name = "CARD")
@Data // Genera getters, setters, toString, equals y hashCode
@NoArgsConstructor // Constructor sin parámetros requerido por JPA
@AllArgsConstructor // Constructor con todos los parámetros
@Builder // Patrón Builder para construcción fluida de objetos
public class Card {
  /**
   * Número de tarjeta - equivalente a CARD-NUM PIC X(16)
   * Clave primaria de la entidad
   */
  @Id
  @Column(name = "CARD_NUM", length = 16, nullable = false)
  private String cardNumber;

  /**
   * ID de cuenta asociada - equivalente a CARD-ACCT-ID PIC 9(11)
   * Referencia a la cuenta bancaria
   */
  @Column(name = "CARD_ACCT_ID", precision = 11, nullable = false)
  private Long accountId;

  /**
   * Código de verificación de tarjeta - equivalente a CARD-CVV-CD PIC 9(03)
   * Código de seguridad de 3 dígitos
   */
  @Column(name = "CARD_CVV_CD", precision = 3, nullable = false)
  private Integer cvvCode;

  /**
   * Nombre grabado en la tarjeta - equivalente a CARD-EMBOSSED-NAME PIC X(50)
   */
  @Column(name = "CARD_EMBOSSED_NAME", length = 50)
  private String embossedName;

  /**
   * Fecha de expiración - equivalente a CARD-EXPIRAION-DATE PIC X(10)
   * Convertido de String a LocalDate para mejor manejo de fechas
   */
  @Column(name = "CARD_EXPIRATION_DATE")
  private LocalDate expirationDate;

  /**
   * Estado activo de la tarjeta - equivalente a CARD-ACTIVE-STATUS PIC X(01)
   * Convertido a enum para mejor control de valores válidos
   */
  @Enumerated(EnumType.STRING)
  @Column(name = "CARD_ACTIVE_STATUS", length = 1)
  private CardStatus activeStatus;

  // Relación N:1 con Account - Múltiples cards pueden pertenecer a una account
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "CARD_ACCT_ID", insertable = false, updatable = false)
  private Account account;

  // Relación 1:N con DailyTransaction - Una card puede tener múltiples transacciones diarias
  @OneToMany(mappedBy = "card", fetch = FetchType.LAZY)
  private List<DailyTransaction> dailyTransactions;

  /**
   * Enum para el estado de la tarjeta
   * Proporciona type safety y valores controlados
   */
  public enum CardStatus {
    A("A", "Active"),
    I("I", "Inactive"),
    B("B", "Blocked"),
    E("E", "Expired");

    private final String code;
    private final String description;

    CardStatus(String code, String description) {
      this.code = code;
      this.description = description;
    }

    public String getCode() {
      return code;
    }

    public String getDescription() {
      return description;
    }

    /**
     * Método utilitario para obtener el enum desde el código COBOL
     */
    public static CardStatus fromCode(String code) {
      for (CardStatus status : values()) {
        if (status.code.equals(code)) {
          return status;
        }
      }
      throw new IllegalArgumentException("Invalid card status code: " + code);
    }
  }
}
