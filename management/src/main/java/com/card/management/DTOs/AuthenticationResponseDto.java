package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para respuesta de autenticación
 * 
 * Contiene el token JWT y la información del usuario autenticado
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AuthenticationResponseDto {

    /**
     * Token JWT de acceso
     */
    private String accessToken;

    /**
     * Token de refresh para obtener un nuevo token de acceso
     */
    private String refreshToken;

    /**
     * Tipo de token (Bearer)
     */
    @Builder.Default
    private String tokenType = "Bearer";

    /**
     * ID del usuario autenticado
     */
    private String userId;

    /**
     * Nombre completo del usuario
     */
    private String fullName;

    /**
     * Tipo de usuario (A=Admin, U=User)
     */
    private String userType;

    /**
     * Tiempo de expiración del token en milisegundos
     */
    private Long expiresIn;

    /**
     * Mensaje de éxito
     */
    private String message;
}
