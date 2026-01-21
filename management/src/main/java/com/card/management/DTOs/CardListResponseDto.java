package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;

import com.card.management.Models.Card.CardStatus;

import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CardListResponseDto {
  private String accountNumber;
  private String cardNumber;
  private CardStatus cardStatus;
}
