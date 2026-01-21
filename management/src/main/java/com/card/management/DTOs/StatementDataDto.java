package com.card.management.DTOs;

import java.math.BigDecimal;
import java.util.List;

import com.card.management.Models.Account;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.Customer;
import com.card.management.Models.TransactionReport;

import lombok.Data;

@Data
public class StatementDataDto {
  private CardXrefRecord cardXref;
  private Customer customer;
  private Account account;
  private List<TransactionReport> transactions;
  private BigDecimal totalAmount;
}
