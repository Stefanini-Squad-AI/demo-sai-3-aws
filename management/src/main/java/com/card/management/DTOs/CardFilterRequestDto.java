package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CardFilterRequestDto {
  private Long accountId;
  private String cardNumber;
  private Integer pageNumber = 1;
  private Integer pageSize = 7;
}
