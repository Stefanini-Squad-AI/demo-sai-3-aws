package com.card.management.Controllers;

import com.card.management.DTOs.AuthenticationRequestDto;
import com.card.management.DTOs.AuthenticationResponseDto;
import com.card.management.Services.AuthenticationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Controlador para gestionar la autenticación de usuarios
 * 
 * Endpoints:
 * - POST /api/auth/login - Autenticar usuario y obtener token JWT
 * - POST /api/auth/refresh - Refrescar token de acceso
 * - POST /api/auth/validate - Validar token JWT
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Authentication", description = "Endpoints para autenticación y gestión de tokens JWT")
public class AuthenticationController {

  private final AuthenticationService authenticationService;

  /**
   * Endpoint de login - Autentica un usuario y devuelve tokens JWT
   * 
   * @param request Credenciales del usuario (userId y password)
   * @return AuthenticationResponseDto con tokens JWT si la autenticación es
   *         exitosa
   */
  @PostMapping("/login")
  @Operation(summary = "Autenticar usuario", description = "Autentica un usuario con sus credenciales y devuelve tokens JWT (access token y refresh token)")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Autenticación exitosa", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthenticationResponseDto.class))),
      @ApiResponse(responseCode = "401", description = "Credenciales inválidas", content = @Content(mediaType = "application/json")),
      @ApiResponse(responseCode = "400", description = "Datos de entrada inválidos", content = @Content(mediaType = "application/json"))
  })
  public ResponseEntity<?> login(@Valid @RequestBody AuthenticationRequestDto request) {
    log.info("Solicitud de login recibida para usuario: {}", request.getUserId());

    AuthenticationResponseDto response = authenticationService.authenticate(request);

    if (response == null) {
      log.warn("Autenticación fallida para usuario: {}", request.getUserId());
      Map<String, String> error = new HashMap<>();
      error.put("error", "Invalid credentials");
      error.put("message", "User ID or password is incorrect");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(error);
    }

    log.info("Usuario autenticado exitosamente: {}", request.getUserId());
    return ResponseEntity.ok(response);
  }

  /**
   * Endpoint para refrescar el access token usando un refresh token válido
   * 
   * @param refreshTokenRequest Mapa con el refresh token
   * @return Nuevo access token
   */
  @PostMapping("/refresh")
  @Operation(summary = "Refrescar access token", description = "Genera un nuevo access token usando un refresh token válido")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Token refrescado exitosamente", content = @Content(mediaType = "application/json")),
      @ApiResponse(responseCode = "401", description = "Refresh token inválido o expirado", content = @Content(mediaType = "application/json"))
  })
  public ResponseEntity<?> refreshToken(@RequestBody Map<String, String> refreshTokenRequest) {
    String refreshToken = refreshTokenRequest.get("refreshToken");

    if (refreshToken == null || refreshToken.isEmpty()) {
      Map<String, String> error = new HashMap<>();
      error.put("error", "Invalid request");
      error.put("message", "Refresh token is required");
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
    }

    String newAccessToken = authenticationService.refreshAccessToken(refreshToken);

    if (newAccessToken == null) {
      Map<String, String> error = new HashMap<>();
      error.put("error", "Invalid token");
      error.put("message", "Refresh token is invalid or expired");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(error);
    }

    Map<String, String> response = new HashMap<>();
    response.put("accessToken", newAccessToken);
    response.put("tokenType", "Bearer");
    response.put("message", "Token refreshed successfully");

    return ResponseEntity.ok(response);
  }

  /**
   * Endpoint para validar un token JWT
   * 
   * @param tokenRequest Mapa con el token a validar
   * @return Resultado de la validación
   */
  @PostMapping("/validate")
  @Operation(summary = "Validar token JWT", description = "Valida si un token JWT es válido y no ha expirado")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Validación completada", content = @Content(mediaType = "application/json"))
  })
  public ResponseEntity<?> validateToken(@RequestBody Map<String, String> tokenRequest) {
    String token = tokenRequest.get("token");

    if (token == null || token.isEmpty()) {
      Map<String, Object> response = new HashMap<>();
      response.put("valid", false);
      response.put("message", "Token is required");
      return ResponseEntity.ok(response);
    }

    try {
      String userId = authenticationService.getUserIdFromToken(token);

      if (userId == null) {
        Map<String, Object> response = new HashMap<>();
        response.put("valid", false);
        response.put("message", "Invalid token");
        return ResponseEntity.ok(response);
      }

      boolean isValid = authenticationService.validateToken(token, userId);

      Map<String, Object> response = new HashMap<>();
      response.put("valid", isValid);
      response.put("userId", userId);
      response.put("message", isValid ? "Token is valid" : "Token is invalid or expired");

      return ResponseEntity.ok(response);
    } catch (Exception e) {
      Map<String, Object> response = new HashMap<>();
      response.put("valid", false);
      response.put("message", "Error validating token: " + e.getMessage());
      return ResponseEntity.ok(response);
    }
  }

  /**
   * Endpoint de health check para verificar que el servicio de autenticación está
   * funcionando
   * 
   * @return Status del servicio
   */
  @GetMapping("/health")
  @Operation(summary = "Health check", description = "Verifica que el servicio de autenticación está funcionando correctamente")
  public ResponseEntity<?> healthCheck() {
    Map<String, String> response = new HashMap<>();
    response.put("status", "UP");
    response.put("service", "Authentication Service");
    response.put("message", "Service is running");
    return ResponseEntity.ok(response);
  }
}
