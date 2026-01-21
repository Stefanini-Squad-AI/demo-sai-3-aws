package com.card.management.Repositories;

import com.card.management.Models.PendingAuthDetail;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import java.time.LocalDate;
import java.util.List;

@Repository
public interface PendingAuthDetailRepository extends JpaRepository<PendingAuthDetail, Long> {

  // Busca detalles expirados para una cuenta específica
  @Query("SELECT d FROM PendingAuthDetail d WHERE d.summary.accountId = :accountId " +
      "AND d.authDate <= :expiryDate ORDER BY d.id")
  List<PendingAuthDetail> findExpiredDetailsByAccountId(
      @Param("accountId") Long accountId,
      @Param("expiryDate") LocalDate expiryDate);

  @Modifying
  @Query("DELETE FROM PendingAuthDetail d WHERE d.summary.accountId = :accountId " +
      "AND d.authDate <= :expiryDate")
  void deleteExpiredDetailsByAccountId(@Param("accountId") Long accountId,
      @Param("expiryDate") LocalDate expiryDate);
}
