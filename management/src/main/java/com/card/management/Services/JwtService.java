package com.card.management.Services;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * Servicio para gestión de tokens JWT (JSON Web Tokens)
 * 
 * Este servicio proporciona métodos para:
 * - Generar tokens JWT para usuarios autenticados
 * - Validar tokens JWT
 * - Extraer información de los tokens (claims)
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Service
@Slf4j
public class JwtService {

    @Value("${jwt.secret}")
    private String secretKey;

    @Value("${jwt.expiration}")
    private Long jwtExpiration;

    /**
     * Genera un token JWT para un usuario
     * 
     * @param userId ID del usuario
     * @return Token JWT firmado
     */
    public String generateToken(String userId) {
        Map<String, Object> claims = new HashMap<>();
        return generateToken(claims, userId);
    }

    /**
     * Genera un token JWT con claims adicionales
     * 
     * @param extraClaims Claims adicionales a incluir en el token
     * @param userId ID del usuario (subject del token)
     * @return Token JWT firmado
     */
    public String generateToken(Map<String, Object> extraClaims, String userId) {
        return buildToken(extraClaims, userId, jwtExpiration);
    }

    /**
     * Construye un token JWT
     * 
     * @param extraClaims Claims adicionales
     * @param userId Subject (usuario)
     * @param expiration Tiempo de expiración en milisegundos
     * @return Token JWT firmado
     */
    private String buildToken(
            Map<String, Object> extraClaims,
            String userId,
            long expiration
    ) {
        Date now = new Date();
        Date expiryDate = new Date(now.getTime() + expiration);

        String token = Jwts.builder()
                .claims(extraClaims)
                .subject(userId)
                .issuedAt(now)
                .expiration(expiryDate)
                .signWith(getSignInKey())
                .compact();

        log.debug("Token JWT generado para usuario: {}, expira en: {}", userId, expiryDate);
        return token;
    }

    /**
     * Valida si un token es válido para un usuario específico
     * 
     * @param token Token JWT a validar
     * @param userId ID del usuario
     * @return true si el token es válido, false en caso contrario
     */
    public boolean isTokenValid(String token, String userId) {
        try {
            final String tokenUserId = extractUserId(token);
            boolean isValid = (tokenUserId.equals(userId)) && !isTokenExpired(token);
            
            if (!isValid) {
                log.warn("Token inválido para usuario: {}", userId);
            }
            
            return isValid;
        } catch (Exception e) {
            log.error("Error validando token: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Extrae el ID de usuario (subject) del token
     * 
     * @param token Token JWT
     * @return ID del usuario
     */
    public String extractUserId(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    /**
     * Extrae la fecha de expiración del token
     * 
     * @param token Token JWT
     * @return Fecha de expiración
     */
    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    /**
     * Extrae un claim específico del token
     * 
     * @param token Token JWT
     * @param claimsResolver Función para extraer el claim deseado
     * @param <T> Tipo del claim
     * @return Valor del claim
     */
    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    /**
     * Extrae todos los claims del token
     * 
     * @param token Token JWT
     * @return Claims del token
     */
    private Claims extractAllClaims(String token) {
        return Jwts.parser()
                .verifyWith(getSignInKey())
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    /**
     * Verifica si un token ha expirado
     * 
     * @param token Token JWT
     * @return true si el token ha expirado, false en caso contrario
     */
    private boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    /**
     * Obtiene la clave de firma para los tokens
     * 
     * @return SecretKey para firmar tokens
     */
    private SecretKey getSignInKey() {
        byte[] keyBytes = Decoders.BASE64.decode(secretKey);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    /**
     * Genera un token de refresh con mayor duración
     * 
     * @param userId ID del usuario
     * @return Token de refresh
     */
    public String generateRefreshToken(String userId) {
        Map<String, Object> claims = new HashMap<>();
        claims.put("type", "refresh");
        // Refresh token dura 7 días
        long refreshExpiration = jwtExpiration * 7;
        return buildToken(claims, userId, refreshExpiration);
    }
}
