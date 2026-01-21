package com.card.management.Controllers;

import com.card.management.DTOs.TransactionListRequestDto;
import com.card.management.DTOs.TransactionListResponseDto;
import com.card.management.Services.TransactionListService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/transactions")
@RequiredArgsConstructor
public class TransactionListController {
  private final TransactionListService transactionListService;

  // Equivalente a PROCESS-ENTER-KEY y navegación principal
  @PostMapping("/list")
  public ResponseEntity<TransactionListResponseDto> getTransactionList(@RequestBody TransactionListRequestDto request) {

    // Validar selección de transacción (equivalente a la lógica de selección S/s)
    if (isValidSelection(request.getSelectionFlag(), request.getSelectedTransactionId())) {
      if ("S".equalsIgnoreCase(request.getSelectionFlag())) {
        // Aquí se redirigiría a COTRN01C en el sistema original
        // Por ahora retornamos un mensaje indicando la selección
        TransactionListResponseDto response = new TransactionListResponseDto();
        response.setMessage("Transaction " + request.getSelectedTransactionId() + " selected for details");
        return ResponseEntity.ok(response);
      } else {
        TransactionListResponseDto response = new TransactionListResponseDto();
        response.setErrorMessage("Invalid selection. Valid value is S");
        return ResponseEntity.badRequest().body(response);
      }
    }

    TransactionListResponseDto response = transactionListService.getTransactionList(request);

    if (response.getErrorMessage() != null) {
      return ResponseEntity.badRequest().body(response);
    }

    return ResponseEntity.ok(response);
  }

  // Equivalente a PROCESS-PF7-KEY (página anterior)
  @PostMapping("/previous-page")
  public ResponseEntity<TransactionListResponseDto> getPreviousPage(@RequestBody TransactionListRequestDto request) {
    TransactionListResponseDto response = transactionListService.processPageBackward(request);

    if (response.getErrorMessage() != null) {
      return ResponseEntity.badRequest().body(response);
    }

    return ResponseEntity.ok(response);
  }

  // Equivalente a PROCESS-PF8-KEY (página siguiente)
  @PostMapping("/next-page")
  public ResponseEntity<TransactionListResponseDto> getNextPage(@RequestBody TransactionListRequestDto request) {
    TransactionListResponseDto response = transactionListService.processPageForward(request);

    if (response.getErrorMessage() != null) {
      return ResponseEntity.badRequest().body(response);
    }

    return ResponseEntity.ok(response);
  }

  private boolean isValidSelection(String selectionFlag, String selectedTransactionId) {
    return selectionFlag != null && !selectionFlag.trim().isEmpty() &&
        selectedTransactionId != null && !selectedTransactionId.trim().isEmpty();
  }
}
