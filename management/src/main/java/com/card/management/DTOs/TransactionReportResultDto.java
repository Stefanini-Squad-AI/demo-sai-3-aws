package com.card.management.DTOs;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * DTO para el resultado completo del reporte de transacciones
 * Contiene todas las transacciones, agrupaciones y totales
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Resultado completo del reporte de transacciones con detalles y totales")
public class TransactionReportResultDto {
    
    @Schema(description = "Tipo de reporte (Monthly, Yearly, Custom)")
    private String reportType;
    
    @Schema(description = "Fecha de inicio del reporte")
    private LocalDate startDate;
    
    @Schema(description = "Fecha de fin del reporte")
    private LocalDate endDate;
    
    @Schema(description = "Lista de transacciones agrupadas por cuenta")
    private List<AccountTransactionGroup> accountGroups;
    
    @Schema(description = "Total general de todas las transacciones")
    private BigDecimal grandTotal;
    
    @Schema(description = "Cantidad total de transacciones en el reporte")
    private Long totalTransactionCount;
    
    @Schema(description = "Cantidad de cuentas/tarjetas en el reporte")
    private Integer accountCount;
    
    /**
     * Agrupa transacciones por cuenta/tarjeta
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @Schema(description = "Grupo de transacciones por cuenta")
    public static class AccountTransactionGroup {
        
        @Schema(description = "Número de tarjeta")
        private String cardNumber;
        
        @Schema(description = "ID de cuenta")
        private Long accountId;
        
        @Schema(description = "Lista de transacciones de esta cuenta")
        private List<TransactionReportDetailDto> transactions;
        
        @Schema(description = "Total de esta cuenta")
        private BigDecimal accountTotal;
        
        @Schema(description = "Cantidad de transacciones de esta cuenta")
        private Integer transactionCount;
    }
}

