package com.card.management.Services;

import com.card.management.Models.User;
import com.card.management.Repositories.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Servicio personalizado para cargar detalles de usuario para Spring Security
 * 
 * Implementa UserDetailsService de Spring Security para integrar
 * nuestro modelo de usuarios con el sistema de autenticación.
 * 
 * Mapeo de roles:
 * - userType "A" -> ROLE_ADMIN (acceso completo)
 * - userType "U" -> ROLE_USER (acceso limitado)
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    /**
     * Carga un usuario por su ID para Spring Security
     * 
     * @param userId ID del usuario
     * @return UserDetails con la información del usuario y sus roles
     * @throws UsernameNotFoundException si el usuario no existe
     */
    @Override
    public UserDetails loadUserByUsername(String userId) throws UsernameNotFoundException {
        log.debug("Cargando usuario para autenticación: {}", userId);

        // Buscar usuario en la base de datos
        User user = userRepository.findById(userId.trim())
                .orElseThrow(() -> {
                    log.warn("Usuario no encontrado: {}", userId);
                    return new UsernameNotFoundException("Usuario no encontrado: " + userId);
                });

        log.debug("Usuario encontrado: {} con tipo: {}", userId, user.getUserType());

        // Convertir a UserDetails de Spring Security
        return org.springframework.security.core.userdetails.User.builder()
                .username(user.getUserId())
                .password(user.getPassword())
                .authorities(getAuthorities(user))
                .accountExpired(false)
                .accountLocked(false)
                .credentialsExpired(false)
                .disabled(false)
                .build();
    }

    /**
     * Obtiene las autoridades (roles) del usuario basándose en userType
     * 
     * @param user Usuario
     * @return Colección de autoridades/roles
     */
    private Collection<? extends GrantedAuthority> getAuthorities(User user) {
        List<GrantedAuthority> authorities = new ArrayList<>();

        String userType = user.getUserType();

        if (userType == null) {
            log.warn("Usuario {} no tiene userType definido, asignando ROLE_USER por defecto", user.getUserId());
            authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
            return authorities;
        }

        // Mapear userType a roles de Spring Security
        switch (userType.toUpperCase()) {
            case "A":
                authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
                log.debug("Usuario {} tiene rol: ROLE_ADMIN", user.getUserId());
                break;
            case "U":
                authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
                log.debug("Usuario {} tiene rol: ROLE_USER", user.getUserId());
                break;
            default:
                log.warn("Tipo de usuario desconocido '{}' para usuario {}, asignando ROLE_USER", 
                        userType, user.getUserId());
                authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
                break;
        }

        return authorities;
    }
}
