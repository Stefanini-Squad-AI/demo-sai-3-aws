package com.card.management.DTOs;

import lombok.Data;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * DTO para representar datos de usuario en respuestas
 */
@Data
@Schema(description = "Datos del usuario")
public class UserDto {
  @Schema(description = "ID del usuario")
  private String userId;

  @Schema(description = "Nombre del usuario")
  private String firstName;

  @Schema(description = "Apellido del usuario")
  private String lastName;

  @Schema(description = "Tipo de usuario")
  private String userType;

  // No incluimos password por seguridad
}
