package com.card.management.batch.steps;

import com.card.management.DTOs.TransactionTypeOperationDto;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;

@Component
public class TransactionTypeMaintenanceWriter implements ItemWriter<TransactionTypeOperationDto> {
    
    @Override
    public void write(Chunk<? extends TransactionTypeOperationDto> chunk) throws Exception {
        // El procesamiento real se hace en el Processor
        // Este writer solo confirma que el chunk fue procesado
        // Equivalente al commit implícito después de cada operación SQL en COBOL
    }
}
