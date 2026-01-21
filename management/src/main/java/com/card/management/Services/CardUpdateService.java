package com.card.management.Services;

import com.card.management.DTOs.CardUpdateRequestDto;
import com.card.management.DTOs.CardDetailResponseDto;
import com.card.management.DTOs.CardSearchRequestDto;
import com.card.management.Models.Card;
import com.card.management.Repositories.CardRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.time.LocalDate;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class CardUpdateService {
  private final CardRepository cardRepository;

  /**
   * Busca los detalles de una tarjeta de crédito
   * Equivalente a la sección 9000-READ-DATA del COBOL
   */
  @Transactional(readOnly = true)
  public CardDetailResponseDto searchCardDetails(CardSearchRequestDto request) {
    log.info("Searching card details for account: {} and card: {}",
        request.getAccountId(), request.getCardNumber());

    Optional<Card> cardOptional = cardRepository
        .findByCardNumber(request.getCardNumber());

    if (cardOptional.isEmpty()) {
      return CardDetailResponseDto.builder()
          .success(false)
          .errorMessage("Did not find cards for this search condition")
          .build();
    }

    Card card = cardOptional.get();

    // Verificar que coincida la cuenta
    if (!card.getAccountId().equals(request.getAccountId())) {
      return CardDetailResponseDto.builder()
          .success(false)
          .errorMessage("Did not find this account in cards database")
          .build();
    }

    return CardDetailResponseDto.builder()
        .cardNumber(card.getCardNumber())
        .accountId(card.getAccountId())
        .embossedName(card.getEmbossedName().toUpperCase())
        .activeStatus(card.getActiveStatus())
        .expiryMonth(card.getExpirationDate().getMonthValue() < 10 ?
              "0" + card.getExpirationDate().getMonthValue() :
              String.valueOf(card.getExpirationDate().getMonthValue()))
          .expiryYear(String.valueOf(card.getExpirationDate().getYear()))
        .success(true)
        .infoMessage("Details of selected card shown above")
        .build();
  }

  /**
   * Actualiza los detalles de una tarjeta de crédito
   * Equivalente a la sección 9200-WRITE-PROCESSING del COBOL
   */
  @Transactional
  public CardDetailResponseDto updateCardDetails(CardUpdateRequestDto request) {
    log.info("Updating card details for card: {}", request.getCardNumber());

    try {
      // Buscar y bloquear el registro para actualización
      Optional<Card> cardOptional = cardRepository
          .findByCardNumber(request.getCardNumber());

      if (cardOptional.isEmpty()) {
        return CardDetailResponseDto.builder()
            .success(false)
            .infoMessage("Could not lock record for update")
            .build();
      }

      Card existingCard = cardOptional.get();

      // Verificar si los datos han cambiado (equivalente a 9300-CHECK-CHANGE-IN-REC)
      if (hasRecordChanged(existingCard, request)) {
        return CardDetailResponseDto.builder()
            .success(false)
            .infoMessage("Record changed by some one else. Please review")
            .build();
      }

      // Actualizar los campos
      existingCard.setEmbossedName(request.getEmbossedName().toUpperCase());
      existingCard.setActiveStatus(request.getActiveStatus());
      existingCard.setExpirationDate(LocalDate.of(
          request.getExpiryYear(),
          request.getExpiryMonth(),
          request.getExpiryDay()));

      // Guardar los cambios
      Card updatedCard = cardRepository.save(existingCard);

      return CardDetailResponseDto.builder()
          .cardNumber(updatedCard.getCardNumber())
          .accountId(updatedCard.getAccountId())
          .embossedName(updatedCard.getEmbossedName())
          .activeStatus(updatedCard.getActiveStatus())
          .expiryMonth(updatedCard.getExpirationDate().getMonthValue() < 10 ?
              "0" + updatedCard.getExpirationDate().getMonthValue() :
              String.valueOf(updatedCard.getExpirationDate().getMonthValue()))
          .expiryYear(String.valueOf(updatedCard.getExpirationDate().getYear()))
          .success(true)
          .infoMessage("Changes committed to database")
          .build();

    } catch (OptimisticLockingFailureException e) {
      log.error("Optimistic locking failure during card update", e);
      return CardDetailResponseDto.builder()
          .success(false)
          .infoMessage("Record changed by some one else. Please review")
          .build();
    } catch (Exception e) {
      log.error("Error updating card details", e);
      return CardDetailResponseDto.builder()
          .success(false)
          .errorMessage("Update of record failed")
          .build();
    }
  }

  /**
   * Verifica si el registro ha sido modificado por otro proceso
   * Equivalente a la sección 9300-CHECK-CHANGE-IN-REC del COBOL
   */
  private boolean hasRecordChanged(Card currentCard, CardUpdateRequestDto request) {
    // Esta lógica dependería de cómo se almacenen los datos originales
    // En el COBOL original se comparan con CCUP-OLD-* fields
    // Aquí se podría implementar usando un hash o timestamp
    return false; // Simplificado para este ejemplo
  }
}
