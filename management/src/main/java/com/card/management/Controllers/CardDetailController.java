package com.card.management.Controllers;

import com.card.management.DTOs.CardDetailRequestDto;
import com.card.management.DTOs.CardDetailResponseDto;
import com.card.management.Services.CardDetailService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/credit-cards")
@RequiredArgsConstructor
@Slf4j
public class CardDetailController {
  private final CardDetailService cardDetailService;

  /**
   * Endpoint para obtener detalles de tarjeta de crédito
   * Equivalente al punto de entrada principal del programa COBOL
   */
  @PostMapping("/details")
  public ResponseEntity<CardDetailResponseDto> getCreditCardDetails(
      @RequestBody CardDetailRequestDto request) {

    log.info("Received credit card detail request: {}", request);

    try {
      CardDetailResponseDto response = cardDetailService
          .processCardDetailRequest(request);

      if (response.isSuccess()) {
        return ResponseEntity.ok(response);
      } else {
        return ResponseEntity.badRequest().body(response);
      }

    } catch (Exception e) {
      log.error("Unexpected error processing request: {}", e.getMessage(), e);

      CardDetailResponseDto errorResponse = new CardDetailResponseDto();
      errorResponse.setSuccess(false);
      errorResponse.setErrorMessage("Unexpected error occurred");

      return ResponseEntity.internalServerError().body(errorResponse);
    }
  }

  /**
   * Endpoint GET alternativo para búsquedas simples
   */
  @GetMapping("/details")
  public ResponseEntity<CardDetailResponseDto> getCreditCardDetailsByParams(
      @RequestParam(required = false) Long accountId,
      @RequestParam(required = false) String cardNumber) {

    CardDetailRequestDto request = new CardDetailRequestDto(accountId, cardNumber);
    return getCreditCardDetails(request);
  }

}
