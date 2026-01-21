package com.card.management.Repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import com.card.management.Models.TransactionCategoryBalance;
import com.card.management.Models.TransactionCategoryKey;

public interface TransactionCategoryBalanceRepository extends JpaRepository<TransactionCategoryBalance, TransactionCategoryKey> {
  Optional<TransactionCategoryBalance> findByAccountIdAndTypeCodeAndCategoryCode(Long accountId, String typeCode, Integer categoryCode);
}
