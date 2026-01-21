package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDate;

/**
 * Entidad Customer migrada desde estructura COBOL CUSTOMER-RECORD
 * Representa la información completa de un cliente en el sistema CardDemo
 * Longitud original del registro COBOL: 500 caracteres
 */
@Entity
@Table(name = "CUSTOMER")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Customer {
  // CUST-ID PIC 9(09) - ID numérico de 9 dígitos
  @Id
  @Column(name = "CUST_ID", length = 9, nullable = false)
  private Long customerId;

  // CUST-FIRST-NAME PIC X(25) - Nombre del cliente
  @Column(name = "CUST_FIRST_NAME", length = 25)
  private String firstName;

  // CUST-MIDDLE-NAME PIC X(25) - Segundo nombre del cliente
  @Column(name = "CUST_MIDDLE_NAME", length = 25)
  private String middleName;

  // CUST-LAST-NAME PIC X(25) - Apellido del cliente
  @Column(name = "CUST_LAST_NAME", length = 25)
  private String lastName;

  // CUST-ADDR-LINE-1 PIC X(50) - Primera línea de dirección
  @Column(name = "CUST_ADDR_LINE_1", length = 50)
  private String addressLine1;

  // CUST-ADDR-LINE-2 PIC X(50) - Segunda línea de dirección
  @Column(name = "CUST_ADDR_LINE_2", length = 50)
  private String addressLine2;

  // CUST-ADDR-LINE-3 PIC X(50) - Tercera línea de dirección
  @Column(name = "CUST_ADDR_LINE_3", length = 50)
  private String addressLine3;

  // CUST-ADDR-STATE-CD PIC X(02) - Código de estado/provincia
  @Column(name = "CUST_ADDR_STATE_CD", length = 2)
  private String stateCode;

  // CUST-ADDR-COUNTRY-CD PIC X(03) - Código de país
  @Column(name = "CUST_ADDR_COUNTRY_CD", length = 3)
  private String countryCode;

  // CUST-ADDR-ZIP PIC X(10) - Código postal
  @Column(name = "CUST_ADDR_ZIP", length = 10)
  private String zipCode;

  // CUST-PHONE-NUM-1 PIC X(15) - Número de teléfono principal
  @Column(name = "CUST_PHONE_NUM_1", length = 15)
  private String phoneNumber1;

  // CUST-PHONE-NUM-2 PIC X(15) - Número de teléfono secundario
  @Column(name = "CUST_PHONE_NUM_2", length = 15)
  private String phoneNumber2;

  // CUST-SSN PIC 9(09) - Número de Seguro Social (9 dígitos)
  @Column(name = "CUST_SSN", length = 9)
  private String socialSecurityNumber;

  // CUST-GOVT-ISSUED-ID PIC X(20) - ID emitido por el gobierno
  @Column(name = "CUST_GOVT_ISSUED_ID", length = 20)
  private String governmentIssuedId;

  // CUST-DOB-YYYY-MM-DD PIC X(10) - Fecha de nacimiento en formato YYYY-MM-DD
  @Column(name = "CUST_DOB")
  private LocalDate dateOfBirth;

  // CUST-EFT-ACCOUNT-ID PIC X(10) - ID de cuenta para transferencias electrónicas
  @Column(name = "CUST_EFT_ACCOUNT_ID", length = 10)
  private String eftAccountId;

  // CUST-PRI-CARD-HOLDER-IND PIC X(01) - Indicador de portador de tarjeta
  // principal
  @Column(name = "CUST_PRI_CARD_HOLDER_IND", length = 1)
  private String primaryCardHolderIndicator;

  // CUST-FICO-CREDIT-SCORE PIC 9(03) - Puntuación de crédito FICO (3 dígitos)
  @Column(name = "CUST_FICO_CREDIT_SCORE")
  private Integer ficoScore;

  // Métodos de utilidad para trabajar con indicadores booleanos
  public boolean isPrimaryCardHolder() {
    return "Y".equalsIgnoreCase(primaryCardHolderIndicator);
  }

  public void setPrimaryCardHolder(boolean isPrimary) {
    this.primaryCardHolderIndicator = isPrimary ? "Y" : "N";
  }

  // Método para obtener el nombre completo
  public String getFullName() {
    StringBuilder fullName = new StringBuilder();
    if (firstName != null && !firstName.trim().isEmpty()) {
      fullName.append(firstName.trim());
    }
    if (middleName != null && !middleName.trim().isEmpty()) {
      if (fullName.length() > 0)
        fullName.append(" ");
      fullName.append(middleName.trim());
    }
    if (lastName != null && !lastName.trim().isEmpty()) {
      if (fullName.length() > 0)
        fullName.append(" ");
      fullName.append(lastName.trim());
    }
    return fullName.toString();
  }

  // Método para obtener la dirección completa
  public String getFullAddress() {
    StringBuilder address = new StringBuilder();
    if (addressLine1 != null && !addressLine1.trim().isEmpty()) {
      address.append(addressLine1.trim());
    }
    if (addressLine2 != null && !addressLine2.trim().isEmpty()) {
      if (address.length() > 0)
        address.append(", ");
      address.append(addressLine2.trim());
    }
    if (addressLine3 != null && !addressLine3.trim().isEmpty()) {
      if (address.length() > 0)
        address.append(", ");
      address.append(addressLine3.trim());
    }
    return address.toString();
  }
}
