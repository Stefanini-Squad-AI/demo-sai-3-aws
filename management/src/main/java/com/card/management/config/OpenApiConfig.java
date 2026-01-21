package com.card.management.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springdoc.core.models.GroupedOpenApi;

/**
 * Configuración de OpenAPI (Swagger) para la API de Card Management
 * Incluye configuración de seguridad JWT para autenticación
 * 
 * Copyright Amazon.com, Inc. or its affiliates.
 * Licensed under the Apache License, Version 2.0
 */
@Configuration
public class OpenApiConfig {
    
    private static final String SECURITY_SCHEME_NAME = "bearerAuth";
    
    /**
     * Configuración principal de OpenAPI con esquema de seguridad JWT
     */
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("Card Management API")
                        .version("1.0.0")
                        .description("API REST para gestión de tarjetas de crédito con autenticación JWT. " +
                                "Para probar endpoints protegidos:\n" +
                                "1. Haz login en POST /api/auth/login\n" +
                                "2. Copia el accessToken de la respuesta\n" +
                                "3. Haz clic en el botón 'Authorize' arriba\n" +
                                "4. Pega el token (sin 'Bearer') y haz clic en 'Authorize'\n" +
                                "5. Ahora puedes probar los endpoints protegidos")
                        .contact(new Contact()
                                .name("Card Management Team")
                                .email("support@cardmanagement.com"))
                        .license(new License()
                                .name("Apache 2.0")
                                .url("http://www.apache.org/licenses/LICENSE-2.0.html")))
                .components(new Components()
                        .addSecuritySchemes(SECURITY_SCHEME_NAME,
                                new SecurityScheme()
                                        .name(SECURITY_SCHEME_NAME)
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                                        .description("Ingresa el token JWT obtenido del endpoint /api/auth/login")))
                .addSecurityItem(new SecurityRequirement().addList(SECURITY_SCHEME_NAME));
    }
    
    /**
     * Grupo de API pública (endpoints de autenticación)
     */
    @Bean
    public GroupedOpenApi authApi() {
        return GroupedOpenApi.builder()
                .group("1-authentication")
                .pathsToMatch("/api/auth/**")
                .build();
    }
    
    /**
     * Grupo de API de gestión de usuarios (requiere ADMIN)
     */
    @Bean
    public GroupedOpenApi usersApi() {
        return GroupedOpenApi.builder()
                .group("2-users-admin")
                .pathsToMatch("/api/users/**")
                .build();
    }
    
    /**
     * Grupo de API de tarjetas
     */
    @Bean
    public GroupedOpenApi cardsApi() {
        return GroupedOpenApi.builder()
                .group("3-cards")
                .pathsToMatch("/api/cards/**")
                .build();
    }
    
    /**
     * Grupo de API de cuentas
     */
    @Bean
    public GroupedOpenApi accountsApi() {
        return GroupedOpenApi.builder()
                .group("4-accounts")
                .pathsToMatch("/api/accounts/**")
                .build();
    }
    
    /**
     * Grupo de API de transacciones
     */
    @Bean
    public GroupedOpenApi transactionsApi() {
        return GroupedOpenApi.builder()
                .group("5-transactions")
                .pathsToMatch("/api/transactions/**")
                .build();
    }
    
    /**
     * Grupo de API de batch jobs (requiere ADMIN)
     */
    @Bean
    public GroupedOpenApi batchJobsApi() {
        return GroupedOpenApi.builder()
                .group("6-batch-jobs-admin")
                .pathsToMatch("/api/batch/**")
                .build();
    }
    
    /**
     * Grupo con todos los endpoints
     */
    @Bean
    public GroupedOpenApi allApi() {
        return GroupedOpenApi.builder()
                .group("all-endpoints")
                .pathsToMatch("/api/**")
                .build();
    }
}