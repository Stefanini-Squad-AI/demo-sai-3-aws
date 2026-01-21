package com.card.management.DTOs;

import lombok.Data;

@Data
public class TransactionTypeOperationDto {
  private String operationType; // A, U, D, *
  private String trType;
  private String trDescription;

  // Constructor que parsea la línea del archivo de entrada
  // Formato: Columna 1=Tipo, Columnas 2-3=Número, Columnas 4-53=Descripción
  public TransactionTypeOperationDto(String inputLine) {
    if (inputLine != null && inputLine.length() >= 3) {
      this.operationType = inputLine.substring(0, 1);
      this.trType = inputLine.substring(1, 3).trim();
      if (inputLine.length() > 3) {
        this.trDescription = inputLine.substring(3).trim();
      }
    }
  }
}
