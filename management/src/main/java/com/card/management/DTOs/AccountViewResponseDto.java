package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * DTO para respuestas de visualización de cuenta
 * Equivalente a los campos de salida del mapa COBOL CACTVWAO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountViewResponseDto {
  
  // Campos de control de pantalla
  /** Fecha actual del sistema en formato ISO */
  private String currentDate; // Equivalente a CURDATEO
  
  /** Hora actual del sistema en formato HH:mm:ss */
  private String currentTime; // Equivalente a CURTIMEO
  
  /** Identificador de la transacción actual */
  private String transactionId; // Equivalente a TRNNAMEO
  
  /** Nombre del programa COBOL equivalente */
  private String programName; // Equivalente a PGMNAMEO

  // Campos de entrada
  /** ID de la cuenta consultada */
  private Long accountId; // Equivalente a ACCTSIDO

  // Datos de la cuenta
  /** Estado actual de la cuenta */
  private String accountStatus; // Equivalente a ACSTTUSO
  
  /** Balance actual de la cuenta en USD */
  private BigDecimal currentBalance; // Equivalente a ACURBALO
  
  /** Límite de crédito autorizado en USD */
  private BigDecimal creditLimit; // Equivalente a ACRDLIMO
  
  /** Límite de crédito para avances en efectivo en USD */
  private BigDecimal cashCreditLimit; // Equivalente a ACSHLIMO
  
  /** Total de créditos aplicados en el ciclo actual */
  private BigDecimal currentCycleCredit; // Equivalente a ACRCYCRO
  
  /** Total de débitos aplicados en el ciclo actual */
  private BigDecimal currentCycleDebit; // Equivalente a ACRCYDBO
  
  /** Fecha de apertura de la cuenta */
  private LocalDate openDate; // Equivalente a ADTOPENO
  
  /** Fecha de expiración de la tarjeta */
  private LocalDate expirationDate; // Equivalente a AEXPDTO
  
  /** Fecha de última reemisión de tarjeta */
  private LocalDate reissueDate; // Equivalente a AREISDTO
  
  /** Identificador del grupo de descuentos */
  private String groupId; // Equivalente a AADDGRPO

  // Datos del cliente
  /** ID único del cliente propietario */
  private Long customerId; // Equivalente a ACSTNUMO
  
  /** Número de Seguro Social enmascarado */
  private String customerSsn; // Equivalente a ACSTSSNO
  
  /** Puntaje FICO crediticio del cliente */
  private Integer ficoScore; // Equivalente a ACSTFCOO
  
  /** Fecha de nacimiento del cliente */
  private LocalDate dateOfBirth; // Equivalente a ACSTDOBO
  
  /** Primer nombre del cliente */
  private String firstName; // Equivalente a ACSFNAMO
  
  /** Segundo nombre del cliente */
  private String middleName; // Equivalente a ACSMNAMO
  
  /** Apellido paterno del cliente */
  private String lastName; // Equivalente a ACSLNAMO
  
  /** Primera línea de dirección */
  private String addressLine1; // Equivalente a ACSADL1O
  
  /** Segunda línea de dirección (opcional) */
  private String addressLine2; // Equivalente a ACSADL2O
  /** Ciudad de residencia */
  private String city; // Equivalente a ACSCITYO
  
  /** Estado, departamento o provincia */
  private String state; // Equivalente a ACSSTTEO
  
  /** Código postal o ZIP */
  private String zipCode; // Equivalente a ACSZIPCO
  
  /** País de residencia */
  private String country; // Equivalente a ACSCTRYO
  
  /** Número de teléfono principal */
  private String phoneNumber1; // Equivalente a ACSPHN1O
  
  /** Número de teléfono alternativo o celular */
  private String phoneNumber2; // Equivalente a ACSPHN2O
  
  /** Número de documento de identidad gubernamental */
  private String governmentId; // Equivalente a ACSGOVTO
  
  /** Identificador de cuenta para transferencias electrónicas */
  private String eftAccountId; // Equivalente a ACSEFTCO
  
  /** Indica si es el tarjetahabiente principal */
  private String primaryCardHolderFlag; // Equivalente a ACSPFLGO

  // Datos de referencia cruzada
  /** Número de tarjeta enmascarado para seguridad */
  private String cardNumber; // Equivalente a CDEMO-CARD-NUM

  // Campos de control y mensajes
  /** Mensaje de error en caso de fallo en la operación */
  private String errorMessage; // Equivalente a ERRMSGO
  
  /** Mensaje informativo sobre el resultado de la operación */
  private String infoMessage; // Equivalente a INFOMSGO

  // Flags de validación - equivalentes a WS-*-FLAG
  /** Indica si los datos de entrada son válidos */
  private boolean inputValid; // Equivalente a INPUT-OK/INPUT-ERROR
  
  /** Indica si el filtro de cuenta pasó la validación */
  private boolean accountFilterValid; // Equivalente a FLG-ACCTFILTER-*
  
  /** Indica si el filtro de cliente pasó la validación */
  private boolean customerFilterValid; // Equivalente a FLG-CUSTFILTER-*
  
  /** Indica si se encontró la cuenta en el archivo maestro */
  private boolean foundAccountInMaster; // Equivalente a FOUND-ACCT-IN-MASTER
  
  /** Indica si se encontró el cliente en el archivo maestro */
  private boolean foundCustomerInMaster; // Equivalente a FOUND-CUST-IN-MASTER
}
