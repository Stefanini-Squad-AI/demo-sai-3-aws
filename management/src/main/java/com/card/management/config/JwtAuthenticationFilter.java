package com.card.management.config;

import com.card.management.Services.JwtService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

/**
 * Filtro JWT para autenticación
 * 
 * Este filtro intercepta todas las peticiones HTTP y:
 * 1. Extrae el token JWT del header Authorization
 * 2. Valida el token
 * 3. Autentica al usuario en el contexto de seguridad de Spring
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtService jwtService;
    private final UserDetailsService userDetailsService;

    private static final String AUTHORIZATION_HEADER = "Authorization";
    private static final String BEARER_PREFIX = "Bearer ";

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain
    ) throws ServletException, IOException {
        
        final String authHeader = request.getHeader(AUTHORIZATION_HEADER);
        final String jwt;
        final String userId;

        // Si no hay header de autorización o no empieza con "Bearer ", continuar sin autenticar
        if (authHeader == null || !authHeader.startsWith(BEARER_PREFIX)) {
            filterChain.doFilter(request, response);
            return;
        }

        // Extraer el token JWT (remover "Bearer " del header)
        jwt = authHeader.substring(BEARER_PREFIX.length());

        try {
            // Extraer el userId del token
            userId = jwtService.extractUserId(jwt);

            // Si el token contiene un userId y no hay autenticación previa en el contexto
            if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                
                // Cargar los detalles del usuario desde la base de datos
                UserDetails userDetails = this.userDetailsService.loadUserByUsername(userId);

                // Validar el token
                if (jwtService.isTokenValid(jwt, userDetails.getUsername())) {
                    
                    // Crear el objeto de autenticación
                    UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                            userDetails,
                            null,
                            userDetails.getAuthorities()
                    );

                    // Agregar detalles adicionales del request
                    authToken.setDetails(
                            new WebAuthenticationDetailsSource().buildDetails(request)
                    );

                    // Establecer la autenticación en el contexto de seguridad
                    SecurityContextHolder.getContext().setAuthentication(authToken);
                    
                    log.debug("Usuario autenticado exitosamente: {}", userId);
                }
            }
        } catch (Exception e) {
            log.error("Error al procesar el token JWT: {}", e.getMessage());
            // No lanzar excepción, simplemente continuar sin autenticar
        }

        // Continuar con la cadena de filtros
        filterChain.doFilter(request, response);
    }
}
