package com.card.management.Repositories;

import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.card.management.Models.CardXrefRecord;

public interface CardXrefRecordRepository extends JpaRepository<CardXrefRecord, String> {
  
  @Query("SELECT c FROM CardXrefRecord c WHERE c.accountId = :accountId")
  Optional<CardXrefRecord> findByAccountId(@Param("accountId") Long accountId);

  
  Optional<CardXrefRecord> findByCardNumber(String cardNumber);

  List<CardXrefRecord> findAllByOrderByCardNumber();

}
