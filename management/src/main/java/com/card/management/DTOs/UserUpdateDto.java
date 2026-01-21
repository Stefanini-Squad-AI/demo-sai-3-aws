package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * DTO para operaciones de actualización de usuario
 * Migrado desde las estructuras de pantalla COUSR2AI/COUSR2AO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Datos para actualización de usuario")
public class UserUpdateDto {
  @NotBlank(message = "User ID can NOT be empty...")
  @Size(max = 8)
  @Schema(description = "ID del usuario", example = "USER001")
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
  @Schema(description = "Contraseña del usuario")
  private String password;

  @NotBlank(message = "User Type can NOT be empty...")
  @Size(max = 1)
  @Schema(description = "Tipo de usuario", example = "A")
  private String userType;
}
