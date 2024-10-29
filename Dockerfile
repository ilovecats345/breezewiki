FROM rust:latest

WORKDIR /app

COPY..

RUN cargo build --release

EXPOSE 8080

CMD ["cargo", "run", "--release"]
