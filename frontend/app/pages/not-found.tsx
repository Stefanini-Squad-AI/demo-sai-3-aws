import { Container, Box, Typography, Button, Paper } from "@mui/material";
import { ErrorOutline, Home } from "@mui/icons-material";
import { Link } from "react-router-dom";

export function meta() {
  return [
    { title: "404 - Page Not Found" },
    {
      name: "description",
      content: "The page you're looking for doesn't exist",
    },
  ];
}

export default function NotFoundPage() {
  return (
    <Container maxWidth="md">
      <Box
        sx={{
          minHeight: "100vh",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <Paper
          elevation={3}
          sx={{
            p: 6,
            textAlign: "center",
            borderRadius: 3,
            maxWidth: 500,
          }}
        >
          <ErrorOutline
            sx={{
              fontSize: 80,
              color: "error.main",
              mb: 2,
            }}
          />
          <Typography variant="h2" component="h1" gutterBottom fontWeight={700}>
            404
          </Typography>
          <Typography variant="h5" gutterBottom>
            Page Not Found
          </Typography>
          <Typography variant="body1" color="text.secondary" sx={{ mb: 4 }}>
            The page you're looking for doesn't exist or has been moved.
          </Typography>
          <Button
            component={Link}
            to="/"
            variant="contained"
            size="large"
            startIcon={<Home />}
          >
            Go Home
          </Button>
        </Paper>
      </Box>
    </Container>
  );
}