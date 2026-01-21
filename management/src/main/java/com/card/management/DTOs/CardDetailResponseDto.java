package com.card.management.DTOs;

import com.card.management.Models.Card.CardStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CardDetailResponseDto {
  private Long accountId;
  private String cardNumber;
  private Integer cvvCode;
  private String embossedName;
  private CardStatus activeStatus;
  private String expiryMonth;
  private String expiryYear;
  private String errorMessage;
  private String infoMessage;
  private boolean success;
}
