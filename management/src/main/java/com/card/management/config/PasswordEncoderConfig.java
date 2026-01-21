package com.card.management.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Configuración del codificador de contraseñas para la aplicación
 * 
 * Esta clase configura BCryptPasswordEncoder para encriptar las contraseñas
 * de los usuarios antes de almacenarlas en la base de datos.
 * 
 * BCrypt genera un hash de contraseña seguro que incluye:
 * - Salt aleatorio para cada contraseña
 * - Hash de 60 caracteres de longitud
 * - Resistencia a ataques de fuerza bruta
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Configuration
public class PasswordEncoderConfig {

    /**
     * Crea y configura un BCryptPasswordEncoder
     * 
     * BCrypt utiliza un factor de trabajo (strength) que por defecto es 10.
     * Un valor más alto hace que el hash sea más seguro pero más lento.
     * El valor predeterminado de 10 proporciona un buen equilibrio entre
     * seguridad y rendimiento.
     * 
     * @return PasswordEncoder configurado con BCrypt
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}
