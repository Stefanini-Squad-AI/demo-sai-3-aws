package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionListRequestDto {
  private String transactionId; // Equivalente a TRNIDINI
  private Integer pageNumber;
  private String selectionFlag; // Para manejar las selecciones S/s
  private String selectedTransactionId;
}
