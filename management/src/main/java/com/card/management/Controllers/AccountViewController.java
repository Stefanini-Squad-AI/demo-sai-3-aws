package com.card.management.Controllers;

import com.card.management.DTOs.AccountViewResponseDto;
import com.card.management.Services.AccountViewService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para manejar solicitudes de visualización de cuentas
 * Migrado desde COACTVWC.CBL - Business logic layer
 * 
 * Endpoints disponibles:
 * - POST /api/account-view/process - Procesar vista de cuenta
 * - GET /api/account-view/initialize - Inicializar pantalla
 */
@RestController
@RequestMapping("/api/account-view")
@RequiredArgsConstructor
@Slf4j
public class AccountViewController {
  private final AccountViewService accountViewService;

  /**
   * Procesa solicitudes de visualización de cuenta
   * Equivalente a la función principal del programa COBOL COACTVWC.CBL
   */
  @GetMapping
  public ResponseEntity<AccountViewResponseDto> processAccountView(@RequestParam Long accountId) {

    log.info("Processing account view request for account: {}", accountId);

    try {
      AccountViewResponseDto response = accountViewService.readAccountData(accountId);
      return ResponseEntity.ok(response);
    } catch (Exception e) {
      log.error("Error processing account view request", e);
      AccountViewResponseDto errorResponse = AccountViewResponseDto.builder()
          .errorMessage("Error processing account view request: " + e.getMessage())
          .inputValid(false)
          .build();
      return ResponseEntity.badRequest().body(errorResponse);
    }
  }

  /**
   * Inicializa la pantalla de visualización de cuenta
   * Equivalente a CDEMO-PGM-ENTER en COBOL
   */
  @GetMapping("/initialize")
  public ResponseEntity<AccountViewResponseDto> initializeScreen() {
    log.info("Initializing account view screen");

    AccountViewResponseDto response = accountViewService.initializeAccountViewScreen();
    return ResponseEntity.ok(response);
  }
}
