package com.card.management.Repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.card.management.Models.TransactionType;

public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {
    
    // Buscar por código de tipo de transacción (método explícito)
    @Query("SELECT tt FROM TransactionType tt WHERE tt.transactionTypeCode = :transactionTypeCode")
    Optional<TransactionType> findByTransactionTypeCodeOptional(@Param("transactionTypeCode") String transactionTypeCode);
    
    // Buscar por descripción (búsqueda parcial)
    @Query("SELECT tt FROM TransactionType tt WHERE UPPER(tt.transactionTypeDescription) LIKE UPPER(CONCAT('%', :description, '%'))")
    List<TransactionType> findByDescriptionContaining(@Param("description") String description);
    
    // Buscar por descripción exacta
    @Query("SELECT tt FROM TransactionType tt WHERE tt.transactionTypeDescription = :description")
    Optional<TransactionType> findByTransactionTypeDescription(@Param("description") String description);
    
    // Obtener todos los tipos de transacción ordenados por código
    @Query("SELECT tt FROM TransactionType tt ORDER BY tt.transactionTypeCode")
    List<TransactionType> findAllOrderByTransactionTypeCode();
    
    // Verificar si existe un tipo de transacción
    @Query("SELECT CASE WHEN COUNT(tt) > 0 THEN true ELSE false END FROM TransactionType tt WHERE tt.transactionTypeCode = :transactionTypeCode")
    boolean existsByTransactionTypeCode(@Param("transactionTypeCode") String transactionTypeCode);
    
    // Spring Data JPA creará automáticamente estos métodos basados en el nombre
    Optional<TransactionType> findByTransactionTypeCode(String transactionTypeCode);
    List<TransactionType> findByTransactionTypeDescriptionContainingIgnoreCase(String description);
    boolean existsByTransactionTypeDescription(String description);
}