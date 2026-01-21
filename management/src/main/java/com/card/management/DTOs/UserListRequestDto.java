package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserListRequestDto {
  private String startUserId;
  private int pageNumber;
  private String direction; // "FORWARD", "BACKWARD"
  private String selectedUserId;
  private String selectionFlag; // "U" for Update, "D" for Delete
}
