package com.card.management.Exceptions;

import com.card.management.DTOs.TransactionAddResponseDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
@Slf4j
public class TransactionExceptionHandler {
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<TransactionAddResponseDto> handleValidationExceptions(
      MethodArgumentNotValidException ex) {

    StringBuilder errorMessage = new StringBuilder();
    ex.getBindingResult().getAllErrors().forEach((error) -> {
      String fieldName = ((FieldError) error).getField();
      String message = error.getDefaultMessage();
      errorMessage.append(message).append(" ");
    });

    log.warn("Validation error: {}", errorMessage.toString());
    return ResponseEntity.badRequest()
        .body(TransactionAddResponseDto.error(errorMessage.toString().trim()));
  }

  @ExceptionHandler(IllegalArgumentException.class)
  public ResponseEntity<TransactionAddResponseDto> handleIllegalArgumentException(
      IllegalArgumentException ex) {

    log.warn("Business validation error: {}", ex.getMessage());
    return ResponseEntity.badRequest()
        .body(TransactionAddResponseDto.error(ex.getMessage()));
  }

  @ExceptionHandler(Exception.class)
  public ResponseEntity<TransactionAddResponseDto> handleGenericException(Exception ex) {
    log.error("Unexpected error occurred", ex);
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
        .body(TransactionAddResponseDto.error("Unable to Add Transaction..."));
  }
}
