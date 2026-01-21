package com.card.management.Models;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;



/**
 * Entidad JPA que representa los datos de usuario del sistema de seguridad
 * Migrada desde la estructura COBOL SEC-USER-DATA
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
 */
@Entity
@Table(name = "SEC_USER_DATA")
@Data // Genera getters, setters, toString, equals y hashCode
@NoArgsConstructor // Constructor sin parámetros requerido por JPA
@AllArgsConstructor // Constructor con todos los parámetros
@Builder // Patrón Builder para construcción de objetos
public class User {
  
  @Id
  @Column(name = "SEC_USR_ID", length = 8, nullable = false)
  @NotBlank(message = "User ID cannot be blank")
  @Size(max = 8, message = "User ID cannot exceed 8 characters")
  private String userId; // Equivalente a SEC-USR-ID PIC X(08)

  @Column(name = "SEC_USR_FNAME", length = 20)
  @Size(max = 20, message = "First name cannot exceed 20 characters")
  private String firstName; // Equivalente a SEC-USR-FNAME PIC X(20)

  @Column(name = "SEC_USR_LNAME", length = 20)
  @Size(max = 20, message = "Last name cannot exceed 20 characters")
  private String lastName; // Equivalente a SEC-USR-LNAME PIC X(20)

  @Column(name = "SEC_USR_PWD", length = 255)
  @NotBlank(message = "Password cannot be blank")
  @Size(min = 8, max = 255, message = "Password must be between 8 and 255 characters")
  private String password; // Password encriptado con BCrypt (hash de 60 caracteres)

  @Column(name = "SEC_USR_TYPE", length = 1)
  @Size(max = 1, message = "User type must be 1 character")
  private String userType; // Equivalente a SEC-USR-TYPE PIC X(01)

  // Nota: SEC-USR-FILLER PIC X(23) se omite intencionalmente
  // ya que los campos de relleno en COBOL no tienen equivalente funcional en Java
  
  /**
   * Método de conveniencia para obtener el nombre completo
    * @return firstName + " " + lastName
    */
  public String getFullName() {
    if (firstName == null && lastName == null) {
      return "";
    }
    return String.format("%s %s", 
      firstName != null ? firstName.trim() : "", 
      lastName != null ? lastName.trim() : "").trim();
  }

  /**
   * Método para verificar si el usuario es de tipo administrador
    * Asume que 'A' representa administrador
    * @return true si el tipo de usuario es 'A'
    */
  public boolean isAdminUser() {
    return "A".equalsIgnoreCase(userType);
  }
}
