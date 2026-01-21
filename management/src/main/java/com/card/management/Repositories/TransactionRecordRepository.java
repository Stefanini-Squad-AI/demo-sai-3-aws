package com.card.management.Repositories;

import com.card.management.Models.TransactionRecord;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface TransactionRecordRepository extends JpaRepository<TransactionRecord, String> {
  // Equivalente a STARTBR con GTEQ - buscar desde un ID específico hacia adelante
  @Query("SELECT t FROM TransactionRecord t WHERE t.transactionId >= :transactionId ORDER BY t.transactionId ASC")
  Page<TransactionRecord> findTransactionsFromId(@Param("transactionId") String transactionId, Pageable pageable);

  // Para navegación hacia atrás
  @Query("SELECT t FROM TransactionRecord t WHERE t.transactionId < :transactionId ORDER BY t.transactionId DESC")
  Page<TransactionRecord> findTransactionsBeforeId(@Param("transactionId") String transactionId, Pageable pageable);

  // Buscar todas las transacciones ordenadas
  @Query("SELECT t FROM TransactionRecord t ORDER BY t.transactionId ASC")
  Page<TransactionRecord> findAllTransactionsOrdered(Pageable pageable);

  // Buscar la última transacción por timestamp de procesamiento
  @Query("SELECT t FROM TransactionRecord t ORDER BY t.processedTimestamp DESC, t.transactionId DESC LIMIT 1")
  Optional<TransactionRecord> findLastTransaction();
}
