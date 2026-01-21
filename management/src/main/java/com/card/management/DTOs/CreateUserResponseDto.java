// ===== com/card/management/DTOs/CreateUserResponseDto.java =====
package com.card.management.DTOs;

import lombok.Data;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * DTO para respuestas de operaciones de usuario
 * Migrado desde la lógica de mensajes del programa COBOL COUSR01C
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Schema(description = "Respuesta de operaciones de usuario")
public class CreateUserResponseDto {
  @Schema(description = "Indica si la operación fue exitosa")
  private boolean success;

  @Schema(description = "Mensaje de respuesta", example = "User USER001 has been added ...")
  private String message;

  @Schema(description = "Datos del usuario creado")
  private UserDto user;

  // ✅ CORRECCIÓN: Constructor con solo success y message
  public CreateUserResponseDto(boolean success, String message) {
    this.success = success;
    this.message = message;
    this.user = null;
  }
}