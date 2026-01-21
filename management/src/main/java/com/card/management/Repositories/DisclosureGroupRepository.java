package com.card.management.Repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.card.management.Models.DisclosureGroup;
import com.card.management.Models.DisclosureGroupKey;

public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroupKey> {
    
    // Buscar por ID del grupo de cuenta
    @Query("SELECT dg FROM DisclosureGroup dg WHERE dg.accountGroupId = :accountGroupId")
    List<DisclosureGroup> findByAccountGroupId(@Param("accountGroupId") String accountGroupId);
    
    // Buscar por código de tipo de transacción
    @Query("SELECT dg FROM DisclosureGroup dg WHERE dg.transactionTypeCode = :transactionTypeCode")
    List<DisclosureGroup> findByTransactionTypeCode(@Param("transactionTypeCode") String transactionTypeCode);
    
    // Buscar por código de categoría de transacción
    @Query("SELECT dg FROM DisclosureGroup dg WHERE dg.transactionCategoryCode = :transactionCategoryCode")
    List<DisclosureGroup> findByTransactionCategoryCode(@Param("transactionCategoryCode") Integer transactionCategoryCode);
    
    // Buscar por combinación específica de accountGroupId y transactionTypeCode
    @Query("SELECT dg FROM DisclosureGroup dg WHERE dg.accountGroupId = :accountGroupId AND dg.transactionTypeCode = :transactionTypeCode")
    Optional<DisclosureGroup> findByAccountGroupIdAndTransactionTypeCode(
        @Param("accountGroupId") String accountGroupId, 
        @Param("transactionTypeCode") String transactionTypeCode);
    
    // Buscar grupos de divulgación con tasa de interés mayor a un valor específico
    @Query("SELECT dg FROM DisclosureGroup dg WHERE dg.interestRate > :minRate")
    List<DisclosureGroup> findByInterestRateGreaterThan(@Param("minRate") java.math.BigDecimal minRate);
    
    // Spring Data JPA creará automáticamente estos métodos basados en el nombre
    List<DisclosureGroup> findByAccountGroupIdOrderByTransactionTypeCode(String accountGroupId);
    List<DisclosureGroup> findByTransactionTypeCodeOrderByAccountGroupId(String transactionTypeCode);

    Optional<DisclosureGroup> findByAccountGroupIdAndTransactionTypeCodeAndTransactionCategoryCode(
        String accountGroupId, String transactionTypeCode, Integer transactionCategoryCode);
}