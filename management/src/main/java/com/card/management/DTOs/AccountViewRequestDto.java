package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para solicitudes de visualización de cuenta
 * Equivalente a los campos de entrada del mapa COBOL
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountViewRequestDto {
  
  /**
   * ID único de la cuenta a consultar
   * Equivalente a CC-ACCT-ID
   */
  private Long accountId;
  
  /**
   * Nombre del programa desde el cual se origina la solicitud
   * Equivalente a CA-FROM-PROGRAM
   */
  private String fromProgram;
  
  /**
   * ID de transacción desde la cual se origina la solicitud
   * Equivalente a CA-FROM-TRANID
   */
  private String fromTransactionId;
  
  /**
   * Tecla de función presionada por el usuario
   * Equivalente a CCARD-AID-*
   */
  private String pfKey;
}
