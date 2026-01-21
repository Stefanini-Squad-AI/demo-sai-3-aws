package com.card.management.Repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.card.management.Models.TransactionCategory;
import com.card.management.Models.TransactionCategoryId;

public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategoryId> {
    
    // Buscar todas las categorías de un tipo de transacción específico
    @Query("SELECT tc FROM TransactionCategory tc WHERE tc.transactionTypeCode = :transactionTypeCode")
    List<TransactionCategory> findByTransactionTypeCode(@Param("transactionTypeCode") String transactionTypeCode);
    
    // Buscar por código de categoría específico
    @Query("SELECT tc FROM TransactionCategory tc WHERE tc.transactionCategoryCode = :categoryCode")
    List<TransactionCategory> findByTransactionCategoryCode(@Param("categoryCode") Integer categoryCode);
    
        // Buscar por descripción que contenga una cadena específica
    @Query("SELECT tc FROM TransactionCategory tc WHERE UPPER(tc.transactionCategoryDescription) LIKE UPPER(CONCAT('%', :description, '%'))")
    List<TransactionCategory> findByDescriptionContaining(@Param("description") String description);
    
    // Buscar combinación específica de tipo y categoría
    @Query("SELECT tc FROM TransactionCategory tc WHERE tc.transactionTypeCode = :transactionTypeCode AND tc.transactionCategoryCode = :categoryCode")
    Optional<TransactionCategory> findByTransactionTypeCodeAndCategoryCode(
        @Param("transactionTypeCode") String transactionTypeCode,
        @Param("categoryCode") Integer categoryCode);
    
    // Obtener todas las categorías ordenadas por tipo y luego por categoría
    @Query("SELECT tc FROM TransactionCategory tc ORDER BY tc.transactionTypeCode, tc.transactionCategoryCode")
    List<TransactionCategory> findAllOrderByTypeAndCategory();
    
    // Buscar categorías por tipo de transacción ordenadas por código de categoría
    @Query("SELECT tc FROM TransactionCategory tc WHERE tc.transactionTypeCode = :transactionTypeCode ORDER BY tc.transactionCategoryCode")
    List<TransactionCategory> findByTransactionTypeCodeOrderByCategory(@Param("transactionTypeCode") String transactionTypeCode);
    
    // Verificar si existe una combinación específica
    @Query("SELECT CASE WHEN COUNT(tc) > 0 THEN true ELSE false END FROM TransactionCategory tc WHERE tc.transactionTypeCode = :transactionTypeCode AND tc.transactionCategoryCode = :categoryCode")
    boolean existsByTransactionTypeCodeAndCategoryCode(
        @Param("transactionTypeCode") String transactionTypeCode,
        @Param("categoryCode") Integer categoryCode);
    
    // Spring Data JPA creará automáticamente estos métodos basados en el nombre
    List<TransactionCategory> findByTransactionTypeCodeOrderByTransactionCategoryCode(String transactionTypeCode);
    List<TransactionCategory> findByTransactionCategoryCodeOrderByTransactionTypeCode(Integer categoryCode);
    // List<TransactionCategory> findByTransactionTypeDescriptionContainingIgnoreCase(String description);
}