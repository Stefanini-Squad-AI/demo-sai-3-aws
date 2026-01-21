package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * DTO para la respuesta de eliminación de usuario
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Respuesta de eliminación de usuario")
public class UserDeleteResponseDto {
  @Schema(description = "ID del usuario eliminado")
  private String userId;

  @Schema(description = "Mensaje de confirmación")
  private String message;

  @Schema(description = "Indica si la operación fue exitosa")
  private boolean success;
}
