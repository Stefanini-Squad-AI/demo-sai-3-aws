package com.card.management.Repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.card.management.Models.DailyTransaction;

@Repository
public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, String> {
    
    /**
     * Encuentra todas las transacciones diarias que aún no han sido procesadas
     * (PROCESSED_TIMESTAMP es NULL)
     * Usado por Spring Batch para procesar transacciones pendientes
     */
    List<DailyTransaction> findByProcessedTimestampIsNull();
    
    /**
     * Versión paginada para Spring Batch ItemReader
     */
    Page<DailyTransaction> findByProcessedTimestampIsNull(Pageable pageable);

    /**
     * Busca transacciones procesadas (con timestamp no nulo)
     * Versión con ordenamiento explícito en el nombre
     */
    List<DailyTransaction> findByProcessedTimestampIsNotNullOrderByCardNumberAscProcessedTimestampAsc();
    
    /**
     * Busca transacciones procesadas (con timestamp no nulo)
     * Para uso con Spring Batch ItemReader con sorts map
     */
    Page<DailyTransaction> findByProcessedTimestampIsNotNull(Pageable pageable);
}