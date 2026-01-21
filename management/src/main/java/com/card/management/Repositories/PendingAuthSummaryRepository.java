package com.card.management.Repositories;

import com.card.management.Models.PendingAuthSummary;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface PendingAuthSummaryRepository extends JpaRepository<PendingAuthSummary, Long> {

  // Equivalente a la lectura secuencial del archivo PAUTSUM0 en COBOL
  @Query("SELECT p FROM PendingAuthSummary p ORDER BY p.accountId")
  Page<PendingAuthSummary> findAllOrderByAccountId(Pageable pageable);
}
