package com.card.management.Services;

import com.card.management.DTOs.AuthenticationRequestDto;
import com.card.management.DTOs.AuthenticationResponseDto;
import com.card.management.Models.User;
import com.card.management.Repositories.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * Servicio de autenticación para gestionar login de usuarios
 * 
 * Este servicio maneja la lógica de autenticación:
 * - Verifica credenciales
 * - Genera tokens JWT
 * - Retorna información del usuario autenticado
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AuthenticationService {

  private final UserRepository userRepository;
  private final PasswordEncoder passwordEncoder;
  private final JwtService jwtService;

  @Value("${jwt.expiration}")
  private Long jwtExpiration;

  /**
   * Autentica un usuario y genera tokens JWT
   * 
   * @param request Credenciales del usuario
   * @return AuthenticationResponseDto con tokens y datos del usuario, o null si
   *         falla
   */
  public AuthenticationResponseDto authenticate(AuthenticationRequestDto request) {
    try {
      log.info("Intento de autenticación para usuario: {}", request.getUserId());

      // Buscar usuario
      Optional<User> userOptional = userRepository.findById(request.getUserId().trim());

      if (userOptional.isEmpty()) {
        log.warn("Usuario no encontrado: {}", request.getUserId());
        return null;
      }

      User user = userOptional.get();

      // Verificar contraseña
      if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
        log.warn("Contraseña incorrecta para usuario: {}", request.getUserId());
        return null;
      }

      // Generar tokens
      String accessToken = jwtService.generateToken(user.getUserId());
      String refreshToken = jwtService.generateRefreshToken(user.getUserId());

      log.info("Autenticación exitosa para usuario: {}", request.getUserId());

      // Construir respuesta
      return AuthenticationResponseDto.builder()
          .accessToken(accessToken)
          .refreshToken(refreshToken)
          .tokenType("Bearer")
          .userId(user.getUserId())
          .fullName(user.getFullName())
          .userType(user.getUserType())
          .expiresIn(jwtExpiration)
          .message("Authentication successful")
          .build();

    } catch (Exception e) {
      log.error("Error durante la autenticación: {}", e.getMessage(), e);
      return null;
    }
  }

  /**
   * Valida un token JWT
   * 
   * @param token  Token a validar
   * @param userId ID del usuario
   * @return true si el token es válido, false en caso contrario
   */
  public boolean validateToken(String token, String userId) {
    try {
      return jwtService.isTokenValid(token, userId);
    } catch (Exception e) {
      log.error("Error validando token: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Extrae el userId de un token JWT
   * 
   * @param token Token JWT
   * @return userId extraído del token
   */
  public String getUserIdFromToken(String token) {
    try {
      return jwtService.extractUserId(token);
    } catch (Exception e) {
      log.error("Error extrayendo userId del token: {}", e.getMessage());
      return null;
    }
  }

  /**
   * Genera un nuevo access token usando un refresh token válido
   * 
   * @param refreshToken Token de refresh
   * @return Nuevo access token, o null si el refresh token es inválido
   */
  public String refreshAccessToken(String refreshToken) {
    try {
      String userId = jwtService.extractUserId(refreshToken);

      if (userId != null && jwtService.isTokenValid(refreshToken, userId)) {
        log.info("Generando nuevo access token para usuario: {}", userId);
        return jwtService.generateToken(userId);
      }

      log.warn("Refresh token inválido");
      return null;
    } catch (Exception e) {
      log.error("Error refrescando token: {}", e.getMessage());
      return null;
    }
  }
}
