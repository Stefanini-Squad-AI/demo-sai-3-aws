package com.card.management.Services;

import com.card.management.DTOs.CardFilterRequestDto;
import com.card.management.DTOs.CardListResponseDto;
import com.card.management.Models.Card;
import com.card.management.Repositories.CardRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class CardListService {
  private final CardRepository cardRepository;

  /**
   * Lista tarjetas de crédito basado en filtros
   * Equivalente a la lógica principal del programa COBOL COCRDLIC
   */
  public Page<CardListResponseDto> listCreditCards(
      CardFilterRequestDto filterRequest,
      boolean isAdminUser) {

    log.info("Listing credit cards with filters: {}, isAdmin: {}",
        filterRequest, isAdminUser);

    Pageable pageable = PageRequest.of(
        filterRequest.getPageNumber() - 1, // Spring usa 0-based indexing
        filterRequest.getPageSize());

    Page<Card> cards;

    // Lógica equivalente a las condiciones del COBOL
    if (isAdminUser && filterRequest.getAccountId() == null) {
      // Admin sin filtro de cuenta - mostrar todas las tarjetas
      cards = cardRepository.findCreditCardsWithFilters(
          null,
          filterRequest.getCardNumber(),
          pageable);
    } else if (isAdminUser) {
      // Admin con filtros específicos
      cards = cardRepository.findCreditCardsWithFilters(
          filterRequest.getAccountId(),
          filterRequest.getCardNumber(),
          pageable);
    } else {
      // Usuario no admin - solo sus tarjetas asociadas
      Long accountId = filterRequest.getAccountId();
      if (accountId == null) {
        throw new IllegalArgumentException(
            "Account ID is required for non-admin users");
      }
      cards = cardRepository.findCreditCardsByAccountId(
          accountId,
          pageable);
    }

    // Mapear entidades a DTOs de respuesta
    return cards.map(this::mapToResponse);
  }

  /**
   * Mapea entidad CreditCard a DTO de respuesta
   * Equivalente a la lógica de WS-SCREEN-ROWS del COBOL
   */
  private CardListResponseDto mapToResponse(Card creditCard) {
    return new CardListResponseDto(
        String.format("%011d", creditCard.getAccountId()), // Formato de 11 dígitos
        creditCard.getCardNumber(),
        creditCard.getActiveStatus()
    );
  }

  /**
   * Valida los filtros de entrada
   * Equivalente a las secciones 2210-EDIT-ACCOUNT y 2220-EDIT-CARD del COBOL
   */
  public void validateFilters(CardFilterRequestDto filterRequest) {

    // Validación de Account ID (equivalente a 2210-EDIT-ACCOUNT)
    // if (filterRequest.getAccountId() != null) {
      // String accountStr = filterRequest.getAccountId().toString();
      // if (accountStr.length() != 11) {
      //   throw new IllegalArgumentException(
      //       "ACCOUNT FILTER, IF SUPPLIED MUST BE A 11 DIGIT NUMBER");
      // }
    // }

    // Validación de Card Number (equivalente a 2220-EDIT-CARD)
    if (filterRequest.getCardNumber() != null &&
        !filterRequest.getCardNumber().isEmpty()) {

      if (!filterRequest.getCardNumber().matches("\\d{16}")) {
        throw new IllegalArgumentException(
            "CARD ID FILTER, IF SUPPLIED MUST BE A 16 DIGIT NUMBER");
      }
    }
  }
}
