package com.card.management.DTOs;

import lombok.Data;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * DTO para la creación de usuarios
 * Migrado desde los campos de entrada del mapa COUSR1AI del programa COBOL
 */
@Data
@Schema(description = "Request para crear un nuevo usuario")
public class CreateUserRequestDto {
  @NotBlank(message = "User ID can NOT be empty...")
  @Size(max = 8)
  @Schema(description = "ID único del usuario", example = "USER001")
  private String userId;

  @NotBlank(message = "First Name can NOT be empty...")
  @Size(max = 25)
  @Schema(description = "Nombre del usuario", example = "John")
  private String firstName;

  @NotBlank(message = "Last Name can NOT be empty...")
  @Size(max = 25)
  @Schema(description = "Apellido del usuario", example = "Doe")
  private String lastName;

  @NotBlank(message = "Password can NOT be empty...")
  @Size(max = 8)
  @Schema(description = "Contraseña del usuario", example = "pass123")
  private String password;

  @NotBlank(message = "User Type can NOT be empty...")
  @Size(max = 1)
  @Schema(description = "Tipo de usuario (R=Regular, A=Admin)", example = "R")
  private String userType;
}
