package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserListItemDto {
  private String selection;
  private String userId;
  private String firstName;
  private String lastName;
  private String userType;
}
