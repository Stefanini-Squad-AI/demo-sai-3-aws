package com.card.management.Repositories;

import com.card.management.Models.Card;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CardRepository extends JpaRepository<Card, String> {
    // Equivalente a la lógica de filtrado del COBOL
    @Query("SELECT c FROM Card c WHERE " +
            "(:accountId IS NULL OR c.accountId = :accountId) AND " +
            "(:cardNumber IS NULL OR c.cardNumber = :cardNumber) " +
            "ORDER BY c.cardNumber")
    Page<Card> findCreditCardsWithFilters(
            @Param("accountId") Long accountId,
            @Param("cardNumber") String cardNumber,
            Pageable pageable);

    // Para usuarios no admin - solo tarjetas asociadas a una cuenta específica
    @Query("SELECT c FROM Card c WHERE c.accountId = :accountId ORDER BY c.cardNumber")
    Page<Card> findCreditCardsByAccountId(
            @Param("accountId") Long accountId,
            Pageable pageable);

    // Equivalente a la lectura por número de tarjeta en COBOL
    Optional<Card> findByCardNumber(String cardNumber);

    // Equivalente a la lectura por ID de cuenta (índice alternativo)
//     @Query("SELECT c FROM Card c WHERE c.accountId = :accountId")
//     Optional<Card> findByAccountId(@Param("accountId") Long accountId);

    // Búsqueda combinada por cuenta y tarjeta
    Optional<Card> findByAccountIdAndCardNumber(Long accountId, String cardNumber);
}
