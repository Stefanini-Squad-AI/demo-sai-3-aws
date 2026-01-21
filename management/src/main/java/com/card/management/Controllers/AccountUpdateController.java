package com.card.management.Controllers;

import com.card.management.DTOs.AccountUpdateRequestDto;
import com.card.management.Services.AccountUpdateService;
import com.card.management.Services.AccountValidationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/accounts")
public class AccountUpdateController {
  @Autowired
  private AccountUpdateService accountUpdateService;

  @Autowired
  private AccountValidationService validationService;

  /**
   * Obtiene datos de cuenta para actualización
   * Equivalente a la funcionalidad de lectura en 9000-READ-ACCT
   */
  @GetMapping("/{accountId}")
  public ResponseEntity<?> getAccountForUpdate(@PathVariable Long accountId) {
    try {
      AccountUpdateRequestDto accountData = accountUpdateService.getAccountWithCustomer(accountId);
      return ResponseEntity.ok(accountData);
    } catch (Exception e) {
      return ResponseEntity.badRequest()
          .body(Map.of("error", "Account not found: " + e.getMessage()));
    }
  }

  /**
   * Actualiza datos de cuenta y cliente
   * Equivalente a la funcionalidad principal del programa COBOL
   */
  @PutMapping("/{accountId}")
  public ResponseEntity<?> updateAccount(
      @PathVariable String accountId,
      @RequestBody AccountUpdateRequestDto request) {

    // Validación de entrada (equivalente a 1200-EDIT-MAP-INPUTS)
    List<String> validationErrors = validationService.validateAccountUpdate(request);
    if (!validationErrors.isEmpty()) {
      return ResponseEntity.badRequest()
          .body(Map.of("errors", validationErrors));
    }

    try {
      // Procesamiento de actualización (equivalente a 9600-WRITE-PROCESSING)
      accountUpdateService.updateAccountAndCustomer(request);
      return ResponseEntity.ok(Map.of("message", "Changes committed to database"));

    } catch (Exception e) {
      return ResponseEntity.internalServerError()
          .body(Map.of("error", "Changes unsuccessful: " + e.getMessage()));
    }
  }

}
