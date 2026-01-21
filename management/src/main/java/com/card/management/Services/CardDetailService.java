package com.card.management.Services;

import com.card.management.DTOs.CardDetailRequestDto;
import com.card.management.DTOs.CardDetailResponseDto;
import com.card.management.Models.Card;
import com.card.management.Repositories.CardRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class CardDetailService {
  private final CardRepository cardRepository;

  // Constantes equivalentes a los literales COBOL
  private static final String PROGRAM_NAME = "COCRDSLC";
  private static final String TRANSACTION_ID = "CCDL";

  /**
   * Procesa la solicitud de detalles de tarjeta de crédito
   * Equivalente a la sección 9000-READ-DATA del programa COBOL
   */
  public CardDetailResponseDto processCardDetailRequest(CardDetailRequestDto request) {
    log.info("Processing card detail request for account: {} and card: {}",
        request.getAccountId(), request.getCardNumber());

    CardDetailResponseDto response = new CardDetailResponseDto();

    // Validar entrada - equivalente a 2200-EDIT-MAP-INPUTS
    // String validationError = validateInput(request);
    // if (validationError != null) {
    //   response.setSuccess(false);
    //   response.setErrorMessage(validationError);
    //   return response;
    // }

    try {
      // Buscar tarjeta - equivalente a 9100-GETCARD-BYACCTCARD
      Optional<Card> card = findCardRecord(request);

      if (card.isPresent()) {
        // Mapear datos encontrados - equivalente a FOUND-CARDS-FOR-ACCOUNT
        mapCardRecordToResponse(card.get(), response);
        response.setSuccess(true);
        response.setInfoMessage("Displaying requested details");
      } else {
        // No encontrado - equivalente a DFHRESP(NOTFND)
        response.setSuccess(false);
        response.setErrorMessage("Did not find cards for this search condition");
      }

    } catch (Exception e) {
      log.error("Error reading card data: {}", e.getMessage(), e);
      response.setSuccess(false);
      response.setErrorMessage("Error reading Card Data File");
    }

    return response;
  }

  /**
   * Valida los datos de entrada
   * Equivalente a las secciones 2210-EDIT-ACCOUNT y 2220-EDIT-CARD
   */
  private String validateInput(CardDetailRequestDto request) {
    // Validar ID de cuenta - equivalente a 2210-EDIT-ACCOUNT
    if (request.getAccountId() == null || request.getAccountId() == 0) {
      return "Account number not provided";
    }

    // Validar número de tarjeta - equivalente a 2220-EDIT-CARD
    if (request.getCardNumber() == null || request.getCardNumber().trim().isEmpty()) {
      return "Card number not provided";
    }

    // Validar formato numérico de cuenta
    if (request.getAccountId().toString().length() != 11) {
      return "Account number must be a non zero 11 digit number";
    }

    // Validar formato numérico de tarjeta
    if (!request.getCardNumber().matches("\\d{16}")) {
      return "Card number if supplied must be a 16 digit number";
    }

    return null; // Sin errores
  }

  /**
   * Busca el registro de tarjeta
   * Equivalente a 9100-GETCARD-BYACCTCARD
   */
  private Optional<Card> findCardRecord(CardDetailRequestDto request) {
    // Buscar por número de tarjeta (clave principal)
    return cardRepository.findByCardNumber(request.getCardNumber());
  }

  /**
   * Mapea el registro de tarjeta a la respuesta
   * Equivalente a la configuración de variables de pantalla en COBOL
   */
  private void mapCardRecordToResponse(Card card, CardDetailResponseDto response) {
    response.setAccountId(card.getAccountId());
    response.setCardNumber(card.getCardNumber());
    response.setCvvCode(card.getCvvCode());
    response.setEmbossedName(card.getEmbossedName());
    response.setActiveStatus(card.getActiveStatus());

    // Formatear fecha de expiración - equivalente a CARD-EXPIRAION-DATE-X
    if (card.getExpirationDate() != null) {
      DateTimeFormatter monthFormatter = DateTimeFormatter.ofPattern("MM");
      DateTimeFormatter yearFormatter = DateTimeFormatter.ofPattern("yyyy");

      response.setExpiryMonth(card.getExpirationDate().format(monthFormatter));
      response.setExpiryYear(card.getExpirationDate().format(yearFormatter));
    }
  }
}
