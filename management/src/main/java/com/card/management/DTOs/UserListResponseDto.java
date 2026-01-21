package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserListResponseDto {
  private List<UserListItemDto> users;
  private int pageNumber;
  private boolean hasNextPage;
  private String firstUserId;
  private String lastUserId;
  private String message;
}
