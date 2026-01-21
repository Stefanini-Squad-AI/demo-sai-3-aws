package com.card.management.Controllers;

import com.card.management.DTOs.CardUpdateRequestDto;
import com.card.management.DTOs.CardDetailResponseDto;
import com.card.management.DTOs.CardSearchRequestDto;
import com.card.management.Services.CardUpdateService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/credit-cards")
@RequiredArgsConstructor
@Validated
@Slf4j
public class CardUpdateController {
  private final CardUpdateService cardUpdateService;

  /**
   * Busca los detalles de una tarjeta de crédito
   * Equivalente al flujo de búsqueda inicial del programa COBOL
   */
  @PostMapping("/search")
  public ResponseEntity<CardDetailResponseDto> searchCardDetails(
      @Valid @RequestBody CardSearchRequestDto request) {

    log.info("Received search request for account: {} and card: {}",
        request.getAccountId(), request.getCardNumber());

    CardDetailResponseDto response = cardUpdateService.searchCardDetails(request);

    if (response.isSuccess()) {
      return ResponseEntity.ok(response);
    } else {
      return ResponseEntity.badRequest().body(response);
    }
  }

  /**
   * Actualiza los detalles de una tarjeta de crédito
   * Equivalente al flujo de actualización del programa COBOL
   */
  @PutMapping("/update")
  public ResponseEntity<CardDetailResponseDto> updateCardDetails(
      @Valid @RequestBody CardUpdateRequestDto request) {

    log.info("Received update request for card: {}", request.getCardNumber());

    CardDetailResponseDto response = cardUpdateService.updateCardDetails(request);

    if (response.isSuccess()) {
      return ResponseEntity.ok(response);
    } else {
      return ResponseEntity.badRequest().body(response);
    }
  }
}
