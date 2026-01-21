package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class BillPaymentRequestDto {
  private Long accountId;
  private String confirmation;
}
