package com.card.management.Exceptions;

import com.card.management.DTOs.CreateUserResponseDto;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * Manejador global de excepciones
 * Migrado desde la lógica de manejo de errores del programa COBOL COUSR01C
 */
@RestControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
  /**
   * Maneja errores de validación de entrada
   * Equivalente a las validaciones del párrafo PROCESS-ENTER-KEY
   */
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<CreateUserResponseDto> handleValidationExceptions(
      MethodArgumentNotValidException ex) {

    StringBuilder errorMessage = new StringBuilder();
    ex.getBindingResult().getAllErrors().forEach((error) -> {
      String fieldName = ((FieldError) error).getField();
      String message = error.getDefaultMessage();
      errorMessage.append(message).append(" ");
    });

    log.warn("Error de validación: {}", errorMessage.toString());

    CreateUserResponseDto response = new CreateUserResponseDto(false, errorMessage.toString().trim());
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
  }

  /**
   * Maneja excepciones generales
   * Equivalente al manejo de DFHRESP(OTHER) en el COBOL
   */
  @ExceptionHandler(Exception.class)
  public ResponseEntity<CreateUserResponseDto> handleGenericException(Exception ex) {
    log.error("Error inesperado: {}", ex.getMessage(), ex);

    CreateUserResponseDto response = new CreateUserResponseDto(false, "Unable to Add User...");
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
  }

  /**
   * Maneja estados ilegales (como no modificar datos)
   * Migrado desde "Please modify to update ..."
   */
  @ExceptionHandler(IllegalStateException.class)
  public ResponseEntity<Map<String, String>> handleIllegalStateException(IllegalStateException ex) {
    log.error("Estado ilegal: {}", ex.getMessage());
    Map<String, String> error = new HashMap<>();
    error.put("error", ex.getMessage());
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /**
   * Maneja excepciones de usuario no encontrado
   * Migrado desde DFHRESP(NOTFND)
   */
  @ExceptionHandler(UserNotFoundException.class)
  public ResponseEntity<Map<String, String>> handleUserNotFoundException(UserNotFoundException ex) {
    log.error("Usuario no encontrado: {}", ex.getMessage());
    Map<String, String> error = new HashMap<>();
    error.put("error", ex.getMessage());
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
  }
}
