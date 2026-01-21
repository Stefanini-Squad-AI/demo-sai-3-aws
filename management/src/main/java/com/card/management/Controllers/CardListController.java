package com.card.management.Controllers;

import com.card.management.DTOs.CardFilterRequestDto;
import com.card.management.DTOs.CardListResponseDto;
import com.card.management.Services.CardListService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/credit-cards")
@RequiredArgsConstructor
@Slf4j
public class CardListController {
  private final CardListService cardListService;

  /**
   * Endpoint principal para listar tarjetas de crédito
   * Equivalente al programa COBOL COCRDLIC
   */
  @PostMapping("/list")
  public ResponseEntity<Page<CardListResponseDto>> listCreditCards(
      @RequestBody CardFilterRequestDto filterRequest,
      @RequestHeader(value = "User-Type", defaultValue = "USER") String userType) {

    try {
      log.info("Received request to list credit cards: {}", filterRequest);

      // Validar filtros de entrada
      cardListService.validateFilters(filterRequest);

      // Determinar si es usuario admin (equivalente a CDEMO-USRTYP-USER)
      boolean isAdminUser = "ADMIN".equalsIgnoreCase(userType);

      // Obtener lista de tarjetas
      Page<CardListResponseDto> creditCards = cardListService.listCreditCards(filterRequest, isAdminUser);

      return ResponseEntity.ok(creditCards);

    } catch (IllegalArgumentException e) {
      log.error("Validation error: {}", e.getMessage());
      return ResponseEntity.badRequest().build();

    } catch (Exception e) {
      log.error("Error listing credit cards", e);
      return ResponseEntity.internalServerError().build();
    }
  }
}
