package com.card.management.Repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.card.management.Models.TransactionReport;

public interface TransactionReportRepository extends JpaRepository<TransactionReport, String> {
  List<TransactionReport> findByCardNumberOrderByTransactionId(String cardNumber);
}
